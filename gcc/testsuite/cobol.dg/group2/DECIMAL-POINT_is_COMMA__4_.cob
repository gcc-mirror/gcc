       *> { dg-do run }
       *> { dg-output-file "group2/DECIMAL-POINT_is_COMMA__4_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT    IS COMMA.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC 99V99.
       PROCEDURE        DIVISION.
           MOVE FUNCTION MIN (3, 1,5) TO X.
           DISPLAY X
           END-DISPLAY.
           STOP RUN.

