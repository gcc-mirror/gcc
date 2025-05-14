       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_LOCALE-DATE.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X   PIC   X(32)   VALUE SPACES.
       PROCEDURE        DIVISION.
           MOVE FUNCTION LOCALE-DATE ( "19630302" ) TO X.
           IF X NOT = SPACES
                DISPLAY "OK"
                END-DISPLAY
           END-IF.
           STOP RUN.

