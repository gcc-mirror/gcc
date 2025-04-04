       *> { dg-do run }
       *> { dg-options "-dialect ibm" }
       *> { dg-output-file "group2/FUNCTION_BIGGER-POINTER.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  N                PIC     S9(8) COMP-5 value 0.
       01  P   REDEFINES N  POINTER.
       01  FILLER.
        05 X                PIC      A(4) VALUE "ABC".
        05 E REDEFINES X    PIC      A(1)  OCCURS 4.
       LINKAGE SECTION.
       77  B                PIC      A.

       PROCEDURE        DIVISION.
           set P to address of E(1).

           display FUNCTION trim(x) '.'

           set address of B to p.
           perform until B = SPACE
             display B no advancing
             set p up by 1
             set address of B to p
           end-perform
           display '.'

           set P to address of E(1)
           set address of B to p
           perform until B = SPACES
             display B no advancing
             add 1 to N
             set address of B to p
           end-perform
           display '.'

           STOP RUN.

