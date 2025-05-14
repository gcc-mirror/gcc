       *> { dg-do run }
       *> { dg-xfail-run-if "" { *-*-* }  }
       *> { dg-output-file "group2/Offset_overflow.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01.
          03 X          PIC X(4) VALUE "abcd".
          03 I          PIC 9 VALUE 5.
       PROCEDURE        DIVISION.
           >>TURN EC-ALL CHECKING ON
           DISPLAY X(I:1) NO ADVANCING.
           STOP RUN.

