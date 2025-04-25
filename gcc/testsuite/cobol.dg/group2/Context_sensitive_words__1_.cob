       *> { dg-do run }
       *> { dg-output-file "group2/Context_sensitive_words__1_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  BYTE-LENGTH  PIC 9.
       01  X            CONSTANT AS BYTE-LENGTH OF BYTE-LENGTH.
       PROCEDURE        DIVISION.
           MOVE X TO BYTE-LENGTH.
           DISPLAY BYTE-LENGTH NO ADVANCING
           END-DISPLAY.
           STOP RUN.

