       *> { dg-do run }

       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     prog.
       ENVIRONMENT     DIVISION.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-LAY-RECORD            PIC X(66).
       01  WS-DUMMY                 PIC X(50).
       01  WS-KEYWORD               PIC X(32).
       01  WS-POINTER               PIC 99.
       PROCEDURE       DIVISION.
           MOVE
       '        10  AF-RECORD-TYPE-SEQUENCE-04     PIC   9(05) COMP-3.'
                  TO WS-LAY-RECORD.
           MOVE 1 TO WS-POINTER.
           PERFORM 0001-SUB.
           IF WS-POINTER NOT = 48
              DISPLAY "Expected 48 - Got " WS-POINTER
              END-DISPLAY
           END-IF.
           ADD 7  TO WS-POINTER
           END-ADD.
           PERFORM 0001-SUB.
           IF WS-POINTER NOT = 62
              DISPLAY "Expected 62 - Got " WS-POINTER
              END-DISPLAY
           END-IF.
           PERFORM 0001-SUB.
           IF WS-POINTER NOT = 63
              DISPLAY "Expected 63 - Got " WS-POINTER
              END-DISPLAY
           END-IF.
           STOP RUN.
       0001-SUB.
           UNSTRING WS-LAY-RECORD
                    DELIMITED
                    BY ' PIC '
                    OR ' COMP-3'
                    OR '.'
              INTO WS-DUMMY
              DELIMITER WS-KEYWORD
              POINTER WS-POINTER
           END-UNSTRING.

