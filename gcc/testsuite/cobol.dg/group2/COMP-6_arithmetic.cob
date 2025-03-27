       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/COMP-6_arithmetic.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-99          PIC 99   USAGE COMP-6.
       01 X-999         PIC 999  USAGE COMP-6.
       01 B-99          USAGE BINARY-LONG UNSIGNED.
       01 B-999         USAGE BINARY-LONG UNSIGNED.
       PROCEDURE        DIVISION.
           MOVE   99  TO B-99
           MOVE B-99  TO X-99
           MOVE  123  TO B-999
           MOVE B-999 TO X-999
           ADD  X-99  X-999 GIVING B-99
           END-ADD
           DISPLAY B-99
           END-DISPLAY
           STOP RUN.

