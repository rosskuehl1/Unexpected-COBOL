>>SOURCE FORMAT FREE
*> Snake game in GNU COBOL using ncurses (Linux/macOS terminal)
*> Build: cobc -x -free -lncurses -o snake snake.cob
*> Run:   ./snake
 IDENTIFICATION DIVISION.
 PROGRAM-ID. SNAKE.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WIDTH                 BINARY-SHORT VALUE 50.
 01  HEIGHT                BINARY-SHORT VALUE 22.
 01  MAX-LEN               BINARY-SHORT VALUE 1000.

 01  snake-length          BINARY-SHORT VALUE 5.
 01  head-x                BINARY-SHORT.
 01  head-y                BINARY-SHORT.
 01  snake-x               OCCURS 1000 TIMES BINARY-SHORT.
 01  snake-y               OCCURS 1000 TIMES BINARY-SHORT.

 01  dir-x                 BINARY-SHORT VALUE 1.
 01  dir-y                 BINARY-SHORT VALUE 0.

 01  food-x                BINARY-SHORT VALUE 10.
 01  food-y                BINARY-SHORT VALUE 10.

 01  score                 BINARY-LONG VALUE 0.
 01  game-over-flag        PIC 9 VALUE 0.

 01  k                     BINARY-LONG SIGNED.
 01  i                     BINARY-LONG.
 01  r                     BINARY-LONG.
 01  seed                  BINARY-LONG.
 01  tmp                   BINARY-LONG.
 01  last-row              BINARY-SHORT.
 01  last-col              BINARY-SHORT.

 01  CH-SPACE              BINARY-SHORT VALUE 32.
 01  CH-HASH               BINARY-SHORT VALUE 35.
 01  CH-FOOD               BINARY-SHORT VALUE 42.
 01  CH-HEAD               BINARY-SHORT VALUE 79.      *> 'O'
 01  CH-BODY               BINARY-SHORT VALUE 111.     *> 'o'

 01  KEY-W                 BINARY-LONG VALUE 119.
 01  KEY-A                 BINARY-LONG VALUE 97.
 01  KEY-S                 BINARY-LONG VALUE 115.
 01  KEY-D                 BINARY-LONG VALUE 100.
 01  KEY-Q                 BINARY-LONG VALUE 113.
 01  KEY-UP                BINARY-LONG VALUE 259.
 01  KEY-DOWN              BINARY-LONG VALUE 258.
 01  KEY-LEFT              BINARY-LONG VALUE 260.
 01  KEY-RIGHT             BINARY-LONG VALUE 261.

 01  ERR-CODE              BINARY-LONG VALUE -1.

 01  cur-date.
     05  YYYY              PIC 9(4).
     05  MM                PIC 9(2).
     05  DD                PIC 9(2).
     05  HH                PIC 9(2).
     05  MIN               PIC 9(2).
     05  SS                PIC 9(2).
     05  MS                PIC 9(2).

 *> scratch for PRINT-NUMBER
 01  N-TO-PRINT            BINARY-LONG.
 01  DIGITS                PIC X(12).
 01  ND                    BINARY-SHORT VALUE 0.
 01  QN                    BINARY-LONG.
 01  DN                    BINARY-LONG.
 01  JN                    BINARY-LONG.
 01  POS-X                 BINARY-SHORT.
 01  POS-Y                 BINARY-SHORT.
 01  CURX                  BINARY-SHORT.
 01  stdscr                POINTER.

 PROCEDURE DIVISION.
 MAIN-SECTION.
     PERFORM INIT-CURSES
     PERFORM INIT-GAME

     PERFORM UNTIL game-over-flag = 1
        PERFORM READ-INPUT
        PERFORM UPDATE-STATE
        PERFORM RENDER
        CALL "napms" USING BY VALUE 70
     END-PERFORM

     PERFORM TEARDOWN
     STOP RUN.

 INIT-CURSES.
     CALL "initscr" RETURNING stdscr
     CALL "noecho"
     CALL "cbreak"
     CALL "keypad" USING BY VALUE stdscr, 1   *> enable arrow keys
     CALL "timeout" USING BY VALUE 0     *> non-blocking getch
     CALL "curs_set" USING BY VALUE 0    *> hide cursor
     .

 INIT-GAME.
     *> seed rand() from current time
     ACCEPT cur-date FROM DATE YYYYMMDD
     ACCEPT cur-date FROM TIME
     COMPUTE seed = FUNCTION NUMVAL(HH) * 3600
                   + FUNCTION NUMVAL(MIN) * 60
                   + FUNCTION NUMVAL(SS).
     CALL "srand" USING BY VALUE seed

     COMPUTE head-x = WIDTH / 2
     COMPUTE head-y = HEIGHT / 2

     PERFORM VARYING i FROM 1 BY 1 UNTIL i > snake-length
        MOVE head-x TO snake-x(i)
        MOVE head-y TO snake-y(i)
        SUBTRACT i FROM snake-x(i) GIVING snake-x(i)
     END-PERFORM

     PERFORM PLACE-FOOD
     .

 READ-INPUT.
     CALL "getch" RETURNING k
     IF k NOT = ERR-CODE
        EVALUATE k
           WHEN KEY-W
              IF dir-y = 0
                 MOVE -1 TO dir-y
                 MOVE 0  TO dir-x
              END-IF
           WHEN KEY-UP
              IF dir-y = 0
                 MOVE -1 TO dir-y
                 MOVE 0  TO dir-x
              END-IF
           WHEN KEY-S
              IF dir-y = 0
                 MOVE 1 TO dir-y
                 MOVE 0 TO dir-x
              END-IF
           WHEN KEY-DOWN
              IF dir-y = 0
                 MOVE 1 TO dir-y
                 MOVE 0 TO dir-x
              END-IF
           WHEN KEY-A
              IF dir-x = 0
                 MOVE -1 TO dir-x
                 MOVE 0  TO dir-y
              END-IF
           WHEN KEY-LEFT
              IF dir-x = 0
                 MOVE -1 TO dir-x
                 MOVE 0  TO dir-y
              END-IF
           WHEN KEY-D
              IF dir-x = 0
                 MOVE 1 TO dir-x
                 MOVE 0 TO dir-y
              END-IF
           WHEN KEY-RIGHT
              IF dir-x = 0
                 MOVE 1 TO dir-x
                 MOVE 0 TO dir-y
              END-IF
           WHEN KEY-Q
              MOVE 1 TO game-over-flag
           WHEN OTHER
              CONTINUE
        END-EVALUATE
     END-IF
     .

 UPDATE-STATE.
     *> compute new head position
     COMPUTE head-x = snake-x(1) + dir-x
     COMPUTE head-y = snake-y(1) + dir-y

     *> collision with walls
     IF head-x <= 0 OR head-x >= (WIDTH - 1)
        MOVE 1 TO game-over-flag
        EXIT PARAGRAPH
     END-IF
     IF head-y <= 0 OR head-y >= (HEIGHT - 1)
        MOVE 1 TO game-over-flag
        EXIT PARAGRAPH
     END-IF

     *> collision with self
     PERFORM VARYING i FROM 1 BY 1 UNTIL i > snake-length
        IF snake-x(i) = head-x AND snake-y(i) = head-y
           MOVE 1 TO game-over-flag
           EXIT PERFORM
        END-IF
     END-PERFORM
     IF game-over-flag = 1
        EXIT PARAGRAPH
     END-IF

     *> move body: shift down from tail
     PERFORM VARYING i FROM snake-length BY -1 UNTIL i < 2
        MOVE snake-x(i - 1) TO snake-x(i)
        MOVE snake-y(i - 1) TO snake-y(i)
     END-PERFORM

     MOVE head-x TO snake-x(1)
     MOVE head-y TO snake-y(1)

     *> check food
     IF head-x = food-x AND head-y = food-y
        IF snake-length < MAX-LEN
           ADD 1 TO snake-length
        END-IF
        ADD 10 TO score
        PERFORM PLACE-FOOD
     END-IF
     .

 PLACE-FOOD.
     PERFORM UNTIL 1 = 2
        CALL "rand" RETURNING r
        COMPUTE food-x = FUNCTION MOD(r, WIDTH - 2) + 1
        CALL "rand" RETURNING r
        COMPUTE food-y = FUNCTION MOD(r, HEIGHT - 2) + 1

        *> ensure not on snake
        MOVE 0 TO tmp
        PERFORM VARYING i FROM 1 BY 1 UNTIL i > snake-length
           IF snake-x(i) = food-x AND snake-y(i) = food-y
              MOVE 1 TO tmp
              EXIT PERFORM
           END-IF
        END-PERFORM
        IF tmp = 0
           EXIT PERFORM
        END-IF
     END-PERFORM
     .

 RENDER.
     CALL "clear"

     COMPUTE last-row = HEIGHT - 1
     COMPUTE last-col = WIDTH - 1

     *> draw border (top/bottom)
     PERFORM VARYING i FROM 0 BY 1 UNTIL i >= WIDTH
        CALL "mvaddch" USING BY VALUE 0, i, CH-HASH
        CALL "mvaddch" USING BY VALUE last-row, i, CH-HASH
     END-PERFORM

     *> draw border (left/right)
     PERFORM VARYING i FROM 0 BY 1 UNTIL i >= HEIGHT
        CALL "mvaddch" USING BY VALUE i, 0, CH-HASH
        CALL "mvaddch" USING BY VALUE i, last-col, CH-HASH
     END-PERFORM

     *> draw food
     CALL "mvaddch" USING BY VALUE food-y, food-x, CH-FOOD

     *> draw snake
     CALL "mvaddch" USING BY VALUE snake-y(1), snake-x(1), CH-HEAD
     PERFORM VARYING i FROM 2 BY 1 UNTIL i > snake-length
        CALL "mvaddch" USING BY VALUE snake-y(i), snake-x(i), CH-BODY
     END-PERFORM

     *> draw score text (simple)
     CALL "mvaddch" USING BY VALUE 0, 2, 83     *> 'S'
     CALL "mvaddch" USING BY VALUE 0, 3, 99     *> 'c'
     CALL "mvaddch" USING BY VALUE 0, 4, 111    *> 'o'
     CALL "mvaddch" USING BY VALUE 0, 5, 114    *> 'r'
     CALL "mvaddch" USING BY VALUE 0, 6, 101    *> 'e'
     CALL "mvaddch" USING BY VALUE 0, 7, 58     *> ':'
     CALL "mvaddch" USING BY VALUE 0, 8, 32     *> ' '

     MOVE score TO N-TO-PRINT
     MOVE 0 TO POS-Y
     MOVE 9 TO POS-X
     PERFORM PRINT-NUMBER

     CALL "refresh"
     .

 PRINT-NUMBER.
    *> Render N-TO-PRINT at (POS-Y, POS-X)
    IF N-TO-PRINT = 0
       CALL "mvaddch" USING BY VALUE POS-Y, POS-X, 48
       EXIT PARAGRAPH
    END-IF

    MOVE 0 TO ND
    PERFORM UNTIL N-TO-PRINT = 0
       COMPUTE QN = N-TO-PRINT / 10
       COMPUTE DN = N-TO-PRINT - (QN * 10)
       ADD 1 TO ND
       MOVE FUNCTION CHAR(48 + DN) TO DIGITS(ND:1)
       MOVE QN TO N-TO-PRINT
    END-PERFORM

    PERFORM VARYING JN FROM ND BY -1 UNTIL JN < 1
       COMPUTE CURX = POS-X + (ND - JN)
       CALL "mvaddch" USING BY VALUE POS-Y, CURX,
                              FUNCTION ORD(DIGITS(JN:1))
    END-PERFORM
    .

 TEARDOWN.
     CALL "endwin"
     DISPLAY "Game Over! Final score: " score UPON CONSOLE
     .
 END PROGRAM SNAKE.
