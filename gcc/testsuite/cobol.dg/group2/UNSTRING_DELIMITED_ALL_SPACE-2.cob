       *> { dg-do run }

       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     prog.
       ENVIRONMENT     DIVISION.
       DATA            DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RECORD.
           02 VALUE SPACE           PIC X(04).
           02 VALUE "ABC AND DE"    PIC X(10).
           02 VALUE SPACE           PIC X(07).
           02 VALUE "FG AND HIJ"    PIC X(10).
           02 VALUE SPACE           PIC X(08).
       01  SPACE-2                  PIC X(02) VALUE SPACE.
       01  WS-DUMMY                 PIC X(15).
       01  WS-POINTER               PIC 99.
       PROCEDURE       DIVISION.
           MOVE 1 TO WS-POINTER.
      *
           PERFORM 0001-SUB.
           IF WS-DUMMY NOT = SPACE
              DISPLAY "Expected space - Got " WS-DUMMY
              END-DISPLAY
           END-IF.
           IF WS-POINTER NOT = 5
              DISPLAY "Expected 5 - Got " WS-POINTER
              END-DISPLAY
           END-IF.
      *
           PERFORM 0001-SUB.
           IF WS-DUMMY NOT = "ABC AND DE"
              DISPLAY "Expected ABC AND DE - Got " WS-DUMMY
              END-DISPLAY
           END-IF.
           IF WS-POINTER NOT = 21
              DISPLAY "Expected 21 - Got " WS-POINTER
              END-DISPLAY
           END-IF.
      *
           PERFORM 0001-SUB.
           IF WS-DUMMY NOT = " FG AND HIJ"
              DISPLAY "Expected  FG AND HIJ - Got " WS-DUMMY
              END-DISPLAY
           END-IF.
           IF WS-POINTER NOT = 40
              DISPLAY "Expected 40 - Got " WS-POINTER
              END-DISPLAY
           END-IF.
           STOP RUN.
       0001-SUB.
           UNSTRING WS-RECORD
                    DELIMITED BY ALL SPACE-2
              INTO WS-DUMMY
              POINTER WS-POINTER
           END-UNSTRING.

