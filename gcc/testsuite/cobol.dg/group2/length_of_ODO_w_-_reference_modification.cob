       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  PLINE.
           03  PLINE-LEN PIC S9(4) COMP-5.
           03  PLINE-TEXT.
               04  FILLER    PIC X(1) OCCURS  1 TO 80
                                      DEPENDING ON PLINE-LEN.
       procedure division.
       a-main section.
             MOVE 5                    TO PLINE-LEN
             MOVE 'the first part in'  TO PLINE-TEXT
             MOVE 30                   TO PLINE-LEN
             IF PLINE-TEXT NOT = 'the f'
                DISPLAY 'text1 wrong: ' PLINE-TEXT
                END-DISPLAY
             END-IF
             MOVE 'the first part in'  TO PLINE-TEXT
             MOVE 4                    TO PLINE-LEN
             MOVE 'second'             TO PLINE-TEXT
             MOVE 14                   TO PLINE-LEN
             IF PLINE-TEXT NOT = 'secofirst part'
                DISPLAY 'text2 wrong: ' PLINE-TEXT
                END-DISPLAY
             END-IF
             MOVE 80                   TO PLINE-LEN
             MOVE SPACES               TO PLINE-TEXT
             MOVE 5                    TO PLINE-LEN
             MOVE 'the first part in'  TO PLINE-TEXT (2:)
             MOVE 30                   TO PLINE-LEN
             IF PLINE-TEXT NOT = ' the '
                DISPLAY 'text3 wrong: ' PLINE-TEXT
                END-DISPLAY
             END-IF
             MOVE 'the first part in'  TO PLINE-TEXT (2:)
             MOVE 4                    TO PLINE-LEN
             MOVE 'second'             TO PLINE-TEXT (2:)
             MOVE 14                   TO PLINE-LEN
             IF PLINE-TEXT NOT = ' sec first par'
                DISPLAY 'text4 wrong: ' PLINE-TEXT
                END-DISPLAY
             END-IF
             STOP RUN.

