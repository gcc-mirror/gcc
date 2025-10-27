       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  MYFLD        PIC X(6) VALUE "ABCDEF".
           88  MYFLD88  VALUE "ABCDEF"
               FALSE IS "OKOKOK".
       PROCEDURE        DIVISION.
       ASTART SECTION.
       A01.
           SET MYFLD88 TO FALSE
           IF MYFLD NOT = "OKOKOK"
              DISPLAY MYFLD
              END-DISPLAY
           END-IF
           STOP RUN.

