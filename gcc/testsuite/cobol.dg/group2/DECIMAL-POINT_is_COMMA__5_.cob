       *> { dg-do run }
       *> { dg-output-file "group2/DECIMAL-POINT_is_COMMA__5_.out" }

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
           COMPUTE X=1 + ,1
           END-COMPUTE
           DISPLAY X
           END-DISPLAY.
           COMPUTE X=1*,1
           END-COMPUTE
           DISPLAY X
           END-DISPLAY.
           STOP RUN.

