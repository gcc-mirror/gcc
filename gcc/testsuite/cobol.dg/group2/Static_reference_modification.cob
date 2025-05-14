       *> { dg-do run }
       *> { dg-output-file "group2/Static_reference_modification.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(4) VALUE "abcd".
       PROCEDURE        DIVISION.
           DISPLAY X(1:1) ":" X(1:2) ":" X(1:3) ":" X(1:4) ":" X(1:)
           END-DISPLAY.
           DISPLAY X(2:1) ":" X(2:2) ":" X(2:3) ":" X(2:)
           END-DISPLAY.
           DISPLAY X(3:1) ":" X(3:2) ":" X(3:)
           END-DISPLAY.
           DISPLAY X(4:1) ":" X(4:)
           END-DISPLAY.
           STOP RUN.

