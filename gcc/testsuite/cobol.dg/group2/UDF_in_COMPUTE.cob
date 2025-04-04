       *> { dg-do run }
       *> { dg-output-file "group2/UDF_in_COMPUTE.out" }

       IDENTIFICATION DIVISION.
       FUNCTION-ID. func.

       DATA DIVISION.
       LINKAGE SECTION.
       01  num PIC 999.

       PROCEDURE DIVISION RETURNING num.
           MOVE 100 TO num
           .
       END FUNCTION func.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION func.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  x PIC 999.

       PROCEDURE DIVISION.
           COMPUTE x = 101 + FUNCTION func
           DISPLAY x
           .
       END PROGRAM prog.

