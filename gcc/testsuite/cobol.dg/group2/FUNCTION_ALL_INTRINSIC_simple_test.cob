       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_ALL_INTRINSIC_simple_test.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. phase0.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 work-string pic X(80) VALUE "      ABC      ".
       PROCEDURE DIVISION.
       DISPLAY """" TRIM(work-string) """"
       goback.
       end program phase0.

