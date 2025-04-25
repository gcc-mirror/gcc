       *> { dg-do run }
       *> { dg-output-file "group2/Context_sensitive_words__7_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X               PIC 9 VALUE 0.
       01  AWAY-FROM-ZERO  PIC 9 VALUE 0.
       PROCEDURE        DIVISION.
           COMPUTE X ROUNDED MODE AWAY-FROM-ZERO
                   AWAY-FROM-ZERO = 1.1
           END-COMPUTE
           DISPLAY X AWAY-FROM-ZERO NO ADVANCING
           END-DISPLAY.
           STOP RUN.

