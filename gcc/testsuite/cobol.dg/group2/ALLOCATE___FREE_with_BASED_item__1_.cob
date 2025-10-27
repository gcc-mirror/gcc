       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       LINKAGE          SECTION.
       01  MYFLD        PIC X(6) BASED VALUE "ABCDEF".
       PROCEDURE        DIVISION.
       ASTART SECTION.
       A01.
           ALLOCATE MYFLD INITIALIZED.
           IF MYFLD NOT = "ABCDEF"
              DISPLAY MYFLD
              END-DISPLAY
           END-IF.
           FREE ADDRESS OF MYFLD.
           STOP RUN.

