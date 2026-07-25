      *> Do not edit this generated file.  See README.txt
      *> { dg-do run }
       *> { dg-options "-ffixed-form  -copyext copy -I." }

                 >>source format is free
        IDENTIFICATION DIVISION.
           PROGRAM-ID. fail.
        ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
              REPOSITORY.
                 FUNCTION LENGTH TRIM INTRINSIC.
        PROCEDURE DIVISION.
           DISPLAY "HELLO WORLD.".
        END PROGRAM fail.

