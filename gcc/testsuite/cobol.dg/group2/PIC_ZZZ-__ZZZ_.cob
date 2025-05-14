       *> { dg-do run }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01  X-ZZZN                    PIC ZZZ-.
       01  XZN-RED REDEFINES X-ZZZN  PIC X(4).
       01  X-ZZZP                    PIC ZZZ+.
       01  XZP-RED REDEFINES X-ZZZP  PIC X(4).
       PROCEDURE        DIVISION.
           MOVE -1 TO X-ZZZN.
           IF XZN-RED NOT = "  1-"
              DISPLAY "(" X-ZZZN ")"
              END-DISPLAY
           END-IF.
           MOVE  0 TO X-ZZZN.
           IF XZN-RED NOT = "    "
              DISPLAY "(" X-ZZZN ")"
              END-DISPLAY
           END-IF.
           MOVE +1 TO X-ZZZN.
           IF XZN-RED NOT = "  1 "
              DISPLAY "(" X-ZZZN ")"
              END-DISPLAY
           END-IF.

           MOVE -1 TO X-ZZZP.
           IF XZP-RED NOT = "  1-"
              DISPLAY "(" X-ZZZP ")"
              END-DISPLAY
           END-IF.
           MOVE  0 TO X-ZZZP.
           IF XZP-RED NOT = "    "
              DISPLAY "(" X-ZZZP ")"
              END-DISPLAY
           END-IF.
           MOVE +1 TO X-ZZZP.
           IF XZP-RED NOT = "  1+"
              DISPLAY "(" X-ZZZP ")"
              END-DISPLAY
           END-IF.
           STOP RUN.

