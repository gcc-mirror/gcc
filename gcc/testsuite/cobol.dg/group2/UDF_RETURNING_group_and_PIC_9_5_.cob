       *> { dg-do run }
       *> { dg-output-file "group2/UDF_RETURNING_group_and_PIC_9_5_.out" }

       IDENTIFICATION   DIVISION.
       FUNCTION-ID.     COPYPAR.
       DATA             DIVISION.
       LINKAGE          SECTION.
       01   PARSA.
         02 PAR1 PICTURE X(32).
         02 PAR2 PICTURE X(32).
       01   PARSB.
         02 PAR1 PICTURE X(32).
         02 PAR2 PICTURE X(32).
       PROCEDURE DIVISION USING PARSA RETURNING PARSB.
           MOVE PARSA TO PARSB
           DISPLAY """" PARSB """"
           GOBACK.
       END FUNCTION COPYPAR.
       IDENTIFICATION   DIVISION.
       FUNCTION-ID.     COPYPAR2.
       DATA             DIVISION.
       LINKAGE          SECTION.
       01   PARSB PIC 99999.
       01   PAR5 PIC 99999.
       PROCEDURE DIVISION USING PAR5 RETURNING PARSB.
           MOVE PAR5 TO PARSB
           DISPLAY PARSB
           GOBACK.
       END FUNCTION COPYPAR2.
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       ENVIRONMENT      DIVISION.
       CONFIGURATION    SECTION.
       REPOSITORY.
           FUNCTION     COPYPAR, COPYPAR2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01   PARS1.
         02 PAR1 PICTURE X(32) VALUE "Santa".
         02 PAR2 PICTURE X(32) VALUE "Claus".
       01   PARS2.
         02 PAR1 PICTURE X(32).
         02 PAR2 PICTURE X(32).
       01   PAR5 PICTURE 99999 VALUE 54321.
       PROCEDURE DIVISION.
           MOVE COPYPAR(PARS1) TO PARS2
           DISPLAY """" PARS2 """".
           DISPLAY COPYPAR2(PAR5)
           STOP RUN.
       END PROGRAM prog.

