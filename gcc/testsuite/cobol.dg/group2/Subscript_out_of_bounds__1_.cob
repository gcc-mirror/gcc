       *> { dg-do run }
       *> { dg-xfail-run-if "" { *-*-* }  }
       *> { dg-output-file "group2/Subscript_out_of_bounds__1_.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G.
         02 X           PIC X OCCURS 10.
       01 I             PIC 9 VALUE 0.
       PROCEDURE        DIVISION.
           >>TURN EC-ALL CHECKING ON
           DISPLAY """" X(I) """"
           END-DISPLAY.
           STOP RUN.

