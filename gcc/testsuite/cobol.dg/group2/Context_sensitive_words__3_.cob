       *> { dg-do run }
       *> { dg-output-file "group2/Context_sensitive_words__3_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  YYYYDDD      PIC 9 VALUE 0.
       01  X            PIC X(16).
       PROCEDURE        DIVISION.
           ACCEPT X FROM DAY YYYYDDD
           END-ACCEPT.
           DISPLAY YYYYDDD NO ADVANCING
           END-DISPLAY.
           STOP RUN.

