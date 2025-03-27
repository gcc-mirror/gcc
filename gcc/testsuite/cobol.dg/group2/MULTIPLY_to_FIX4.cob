       *> { dg-do run }
       *> { dg-output-file "group2/MULTIPLY_to_FIX4.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. onsize.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  FIX4DISPLAY                PIC 9(4) DISPLAY.
        01  FIX4PACKED                 PIC 9(4) PACKED-DECIMAL.
        01  FIX4BINARY                 PIC 9(4) BINARY.
        01  FIX4COMP5                  PIC 9(4) COMP-5.
        01  FLTSHORT                            FLOAT-SHORT.
        01  FLTLONG                             FLOAT-LONG.
        01  FLTEXT                              FLOAT-EXTENDED.

        PROCEDURE       DIVISION.

        DISPLAY "Checking size error on FIX4DISPLAY"
        MOVE 1 TO FIX4DISPLAY
        PERFORM 10 TIMEs
            DISPLAY  "    FIX4DISPLAY is : " FIX4DISPLAY
            MULTIPLY 10 BY FIX4DISPLAY
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE1
            END-MULTIPLY
        END-PERFORM.
        DONE1.
        DISPLAY  "    Final       is : " FIX4DISPLAY
        DISPLAY "."

        DISPLAY "Checking size error on FIX4PACKED"
        MOVE 1 TO FIX4PACKED
        PERFORM 10 TIMEs
            DISPLAY  "    FIX4PACKED is : " FIX4PACKED
            MULTIPLY 10 BY FIX4PACKED
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE2
            END-MULTIPLY
        END-PERFORM.
        DONE2.
        DISPLAY  "    Final      is : " FIX4PACKED
        DISPLAY "."

        DISPLAY "Checking size error on FIX4BINARY"
        MOVE 1 TO FIX4BINARY
        PERFORM 10 TIMEs
            DISPLAY  "    FIX4BINARY is : " FIX4BINARY
            MULTIPLY 10 BY FIX4BINARY
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE3
            END-MULTIPLY
        END-PERFORM.
        DONE3.
        DISPLAY  "    Final      is : " FIX4BINARY
        DISPLAY "."

        DISPLAY "Checking size error on FIX4COMP5"
        MOVE 1 TO FIX4COMP5
        PERFORM 10 TIMEs
            DISPLAY  "    FIX4COMP5 is : " FIX4COMP5
            MULTIPLY 10 BY FIX4COMP5
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE4
            END-MULTIPLY
        END-PERFORM.
        DONE4.
        DISPLAY  "    Final     is : " FIX4COMP5
        DISPLAY "."

        DISPLAY "Checking size error on FLTSHORT"
        MOVE 1.E34 TO FLTSHORT
        PERFORM 10 TIMEs
            DISPLAY  "    FLTSHORT is : " FLTSHORT
            MULTIPLY 10 BY FLTSHORT
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE5
            END-MULTIPLY
        END-PERFORM.
        DONE5.
        DISPLAY  "    Final    is : " FLTSHORT
        DISPLAY "."

        MOVE 1.E304 TO FLTLONG
        PERFORM 1000 TIMEs
            DISPLAY  "    FLTLONG is  : " FLTLONG
            MULTIPLY 10 BY FLTLONG
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE6
            END-MULTIPLY
        END-PERFORM.
        DONE6.
        DISPLAY  "    Final   is  : " FLTLONG
        DISPLAY "."

        MOVE 1.E4928 TO FLTEXT
        PERFORM 10 TIMEs
            DISPLAY  "    FLTEXT is  : " FLTEXT
            MULTIPLY 10 BY FLTEXT
                     ON SIZE ERROR DISPLAY "    Got size error" GO TO DONE7
            END-MULTIPLY
        END-PERFORM.
        DONE7.
        DISPLAY  "    Final  is   : " FLTEXT
        DISPLAY ".".

        END PROGRAM onsize.

