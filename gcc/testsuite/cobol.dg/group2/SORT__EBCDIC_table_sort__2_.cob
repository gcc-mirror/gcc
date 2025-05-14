       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           OBJECT-COMPUTER.
             x86 PROGRAM COLLATING SEQUENCE IS EBCDIC-CODE.
       SPECIAL-NAMES.
           ALPHABET EBCDIC-CODE IS EBCDIC.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 Z  PIC X(10)  VALUE "d4b2e1a3c5".
       01 G.
         02 TBL         OCCURS 10.
           03 X         PIC X.
       PROCEDURE        DIVISION.
           MOVE Z TO G.
           SORT TBL ASCENDING KEY X.
           IF G NOT = "abcde12345"
              DISPLAY G.
           MOVE Z TO G.
           SORT TBL DESCENDING KEY X.
           IF G NOT = "54321edcba"
              DISPLAY G.
           STOP RUN.

