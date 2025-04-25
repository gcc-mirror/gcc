       *> { dg-do run }
       *> { dg-output-file "group2/DISPLAY_data_items_with_MOVE_statement.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X-ABC         PIC XXX   VALUE "abc".
       01 X-123         PIC 999   VALUE  123.
       01 X-P123        PIC S999  VALUE +123.
       01 X-N123        PIC S999  VALUE -123.
       01 X-12-3        PIC 99V9  VALUE  12.3.
       01 X-P12-3       PIC S99V9 VALUE +12.3.
       01 X-N12-3       PIC S99V9 VALUE -12.3.
       PROCEDURE        DIVISION.
           MOVE "abc" TO X-ABC.
           DISPLAY X-ABC
           END-DISPLAY.
           MOVE  123  TO X-123.
           DISPLAY X-123
           END-DISPLAY.
           MOVE +123  TO X-P123.
           DISPLAY X-P123
           END-DISPLAY.
           MOVE -123  TO X-N123.
           DISPLAY X-N123
           END-DISPLAY.
           MOVE  12.3 TO X-12-3.
           DISPLAY X-12-3
           END-DISPLAY.
           MOVE +12.3 TO X-P12-3.
           DISPLAY X-P12-3
           END-DISPLAY.
           MOVE -12.3 TO X-N12-3.
           DISPLAY X-N12-3
           END-DISPLAY.
           STOP RUN.

