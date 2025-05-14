       *> { dg-do run }
       *> { dg-xfail-run-if "" { *-*-* }  }
       *> { dg-output-file "group2/Length_overflow_with_offset__3_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 X             PIC X(4) VALUE "abcd".
       01 I             PIC 9 VALUE 3.
       PROCEDURE        DIVISION.
           >>TURN EC-ALL CHECKING ON
           EVALUATE TRUE
           WHEN I < 2
            AND X(3:I) <> SPACES
              DISPLAY "1-" X(3:I) NO ADVANCING
           WHEN I < 2
           WHEN X(3:I) <> SPACES
              DISPLAY "2-" X(3:I) NO ADVANCING
           END-EVALUATE
           STOP RUN.

