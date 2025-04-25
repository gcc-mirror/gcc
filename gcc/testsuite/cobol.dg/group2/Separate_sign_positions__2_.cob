       *> { dg-do run }
       *> { dg-output-file "group2/Separate_sign_positions__2_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC S9 SIGN LEADING SEPARATE.
       01  Y            PIC S9 SIGN TRAILING SEPARATE.
       PROCEDURE        DIVISION.
           MOVE 0 TO X.
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           MOVE ZERO TO X.
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           MOVE 0 TO Y.
           DISPLAY Y NO ADVANCING
           END-DISPLAY.
           MOVE ZERO TO Y.
           DISPLAY Y NO ADVANCING
           END-DISPLAY.
           STOP RUN.

