       *> { dg-do run }
       *> { dg-output-file "group2/Separate_sign_positions__1_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC S9 VALUE -1 SIGN LEADING SEPARATE.
       01  Y            PIC S9 VALUE -1 SIGN TRAILING SEPARATE.
       PROCEDURE        DIVISION.
           DISPLAY X(1:1) X(2:1) NO ADVANCING
           END-DISPLAY.
           DISPLAY Y(1:1) Y(2:1) NO ADVANCING
           END-DISPLAY.
           STOP RUN.

