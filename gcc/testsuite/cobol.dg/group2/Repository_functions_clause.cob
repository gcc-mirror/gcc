       *> { dg-do run }
       *> { dg-output-file "group2/Repository_functions_clause.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
          SOURCE-COMPUTER. a.
          OBJECT-COMPUTER. a.
          REPOSITORY.
             FUNCTION ALL INTRINSIC.
       PROCEDURE DIVISION.
          DISPLAY "OK".

