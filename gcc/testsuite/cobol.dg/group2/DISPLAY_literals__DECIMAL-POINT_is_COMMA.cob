       *> { dg-do run }
       *> { dg-output-file "group2/DISPLAY_literals__DECIMAL-POINT_is_COMMA.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT    IS COMMA.
       PROCEDURE        DIVISION.
           DISPLAY  12,3
           END-DISPLAY.
           DISPLAY +12,3
           END-DISPLAY.
           DISPLAY -12,3
           END-DISPLAY.
           DISPLAY 1,23E0
           END-DISPLAY.
           DISPLAY +1,23E0
           END-DISPLAY.
           DISPLAY -1,23E0
           END-DISPLAY.
           STOP RUN.

