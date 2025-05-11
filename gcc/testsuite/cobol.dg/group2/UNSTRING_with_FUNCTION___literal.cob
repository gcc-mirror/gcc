       *> { dg-do run }
       *> { dg-output-file "group2/UNSTRING_with_FUNCTION___literal.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA  DIVISION.
       WORKING-STORAGE SECTION.
       01  FILLER.
         05  TSTUNS PIC X(479).
         05  PRM    PIC X(16) OCCURS 4 TIMES.
       PROCEDURE DIVISION.
           MOVE "The,Quick,Brown,Fox" TO TSTUNS.
           UNSTRING TSTUNS DELIMITED BY ','
              INTO  PRM(1), PRM(2), PRM(3), PRM(4).
           DISPLAY "PRM(1) is " PRM(1) ":".
           DISPLAY "PRM(2) is " PRM(2) ":".
           DISPLAY "PRM(3) is " PRM(3) ":".
           DISPLAY "PRM(4) is " PRM(4) ":".
           UNSTRING FUNCTION UPPER-CASE(TSTUNS) DELIMITED BY ','
              INTO  PRM(1), PRM(2), PRM(3), PRM(4).
           DISPLAY "Now using UPPER-CASE"
           DISPLAY "PRM(1) is " PRM(1) ":".
           DISPLAY "PRM(2) is " PRM(2) ":".
           DISPLAY "PRM(3) is " PRM(3) ":".
           DISPLAY "PRM(4) is " PRM(4) ":".
           UNSTRING "Daddy,was,a,Rolling stone" DELIMITED BY ','
              INTO  PRM(1), PRM(2), PRM(3), PRM(4).
           DISPLAY "Now using Literal"
           DISPLAY "PRM(1) is " PRM(1) ":".
           DISPLAY "PRM(2) is " PRM(2) ":".
           DISPLAY "PRM(3) is " PRM(3) ":".
           DISPLAY "PRM(4) is " PRM(4) ":".
           UNSTRING FUNCTION LOWER-CASE("Daddy,was,a,Rolling stone")
                DELIMITED BY ','
              INTO  PRM(1), PRM(2), PRM(3), PRM(4).
           DISPLAY "Now using Literal + LOWER-CASE"
           DISPLAY "PRM(1) is " PRM(1) ":".
           DISPLAY "PRM(2) is " PRM(2) ":".
           DISPLAY "PRM(3) is " PRM(3) ":".
           DISPLAY "PRM(4) is " PRM(4) ":".
           STOP RUN.

