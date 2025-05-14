       *> { dg-do run }
       *> { dg-output-file "group2/CALL_with_OMITTED_parameter.out" }

        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      caller.
        DATA             DIVISION.
        WORKING-STORAGE  SECTION.
        01 P1            PIC X    VALUE "A".
        01 P2            PIC X    VALUE "B".
        PROCEDURE        DIVISION.
            DISPLAY "Should see AB"
            CALL "callee" USING P1 P2
            DISPLAY "Should see A"
            CALL "callee" USING P1
            END-CALL.
            DISPLAY "Should see A"
            CALL "callee" USING P1 OMITTED
            END-CALL.
            STOP RUN.
        END PROGRAM caller.
       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      callee.
       DATA             DIVISION.
       LINKAGE          SECTION.
       01 P1            PIC X.
       01 P2            PIC X.
       PROCEDURE        DIVISION USING P1 OPTIONAL P2.
           DISPLAY """" P1 WITH NO ADVANCING
           IF P2 NOT OMITTED
              DISPLAY P2 """"
              END-DISPLAY
           ELSE
              DISPLAY """"
              END-DISPLAY
           END-IF.
           EXIT PROGRAM.
        END PROGRAM callee.

