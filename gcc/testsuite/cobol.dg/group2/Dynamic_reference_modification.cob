       *> { dg-do run }
       *> { dg-output-file "group2/Dynamic_reference_modification.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(4) VALUE "abcd".
       01 I             PIC 9.
       PROCEDURE        DIVISION.
           MOVE 1 TO I.
           DISPLAY X(I:1)
           END-DISPLAY.
           MOVE 4 TO I.
           DISPLAY X(I:1)
           END-DISPLAY.
           MOVE 1 TO I.
           DISPLAY X(1:I)
           END-DISPLAY.
           MOVE 4 TO I.
           DISPLAY X(1:I)
           END-DISPLAY.
           STOP RUN.

