       *> { dg-do run }
       *> { dg-output-file "group2/MOVE_integer_literal_to_alphanumeric.out" }


       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X            PIC X(04) VALUE SPACES.
       PROCEDURE        DIVISION.
           MOVE 0 TO X.
           DISPLAY X NO ADVANCING
           END-DISPLAY.
           STOP RUN.

