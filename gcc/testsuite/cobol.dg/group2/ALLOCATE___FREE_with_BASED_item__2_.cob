       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 MYFLD         BASED.
             03 MYFLDX  PIC X.
             03 MYFLD9  PIC 9.
       PROCEDURE        DIVISION.
           IF ADDRESS OF MYFLD NOT = NULL
              DISPLAY "BASED ITEM WITH ADDRESS ON START"
              END-DISPLAY
           END-IF.
           FREE MYFLD.
           ALLOCATE MYFLD.
           IF ADDRESS OF MYFLD = NULL
              DISPLAY "BASED ITEM WITHOUT ADDRESS AFTER ALLOCATE"
              END-DISPLAY
           END-IF.
           INITIALIZE MYFLD.
           IF MYFLD NOT = " 0"
              DISPLAY "BASED ITEM INITIALIZED WRONG: "
                 WITH NO ADVANCING
              END-DISPLAY
              DISPLAY MYFLD
              END-DISPLAY
           END-IF.

           FREE ADDRESS OF MYFLD.
           IF ADDRESS OF MYFLD NOT = NULL
              DISPLAY "BASED ITEM WITH ADDRESS AFTER FREE"
              END-DISPLAY
           END-IF.

