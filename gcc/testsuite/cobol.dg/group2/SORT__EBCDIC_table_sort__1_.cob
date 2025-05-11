       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           ALPHABET ALPHA IS EBCDIC.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 Z  PIC X(10)  VALUE "d4b2e1a3c5".
       01 G.
         02 TBL         OCCURS 10.
           03 X         PIC X.
       PROCEDURE        DIVISION.
           MOVE Z TO G.
           SORT TBL ASCENDING KEY X SEQUENCE ALPHA.
           IF G NOT = "abcde12345"
              DISPLAY G
              END-DISPLAY
           END-IF.
           MOVE Z TO G.
           SORT TBL DESCENDING KEY X SEQUENCE ALPHA.
           IF G NOT = "54321edcba"
              DISPLAY G
              END-DISPLAY
           END-IF.
           STOP RUN.

