       *> { dg-do run }
       *> { dg-output-file "group2/PACKED-DECIMAL_dump.out" }

       IDENTIFICATION   DIVISION.
       PROGRAM-ID.      prog.
       DATA             DIVISION.
       WORKING-STORAGE  SECTION.
       01 G-1.
         02 X-1         PIC 9(1) VALUE 1
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-2.
         02 X-2         PIC 9(2) VALUE 12
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-3.
         02 X-3         PIC 9(3) VALUE 123
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-4.
         02 X-4         PIC 9(4) VALUE 1234
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-5.
         02 X-5         PIC 9(5) VALUE 12345
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-6.
         02 X-6	        PIC 9(6) VALUE 123456
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-7.
         02 X-7         PIC 9(7) VALUE 1234567
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-8.
         02 X-8         PIC 9(8) VALUE 12345678
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-9.
         02 X-9         PIC 9(9) VALUE 123456789
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-10.
         02 X-10        PIC 9(10) VALUE 1234567890
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-11.
         02 X-11        PIC 9(11) VALUE 12345678901
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-12.
         02 X-12        PIC 9(12) VALUE 123456789012
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-13.
         02 X-13        PIC 9(13) VALUE 1234567890123
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-14.
         02 X-14        PIC 9(14) VALUE 12345678901234
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-15.
         02 X-15        PIC 9(15) VALUE 123456789012345
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-16.
         02 X-16        PIC 9(16) VALUE 1234567890123456
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-17.
         02 X-17        PIC 9(17) VALUE 12345678901234567
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-18.
         02 X-18        PIC 9(18) VALUE 123456789012345678
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S1.
         02 X-S1        PIC S9(1) VALUE -1
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S2.
         02 X-S2        PIC S9(2) VALUE -12
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S3.
         02 X-S3        PIC S9(3) VALUE -123
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S4.
         02 X-S4        PIC S9(4) VALUE -1234
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S5.
         02 X-S5        PIC S9(5) VALUE -12345
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S6.
         02 X-S6        PIC S9(6) VALUE -123456
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S7.
         02 X-S7        PIC S9(7) VALUE -1234567
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S8.
         02 X-S8        PIC S9(8) VALUE -12345678
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S9.
         02 X-S9        PIC S9(9) VALUE -123456789
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S10.
         02 X-S10       PIC S9(10) VALUE -1234567890
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S11.
         02 X-S11       PIC S9(11) VALUE -12345678901
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S12.
         02 X-S12       PIC S9(12) VALUE -123456789012
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S13.
         02 X-S13       PIC S9(13) VALUE -1234567890123
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S14.
         02 X-S14       PIC S9(14) VALUE -12345678901234
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S15.
         02 X-S15       PIC S9(15) VALUE -123456789012345
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S16.
         02 X-S16       PIC S9(16) VALUE -1234567890123456
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S17.
         02 X-S17       PIC S9(17) VALUE -12345678901234567
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       01 G-S18.
         02 X-S18       PIC S9(18) VALUE -123456789012345678
                        COMP-3.
         02 FILLER      PIC X(18) VALUE SPACE.
       PROCEDURE        DIVISION.
      *>   Dump all values
           CALL "dump" USING G-1
           END-CALL.
           CALL "dump" USING G-2
           END-CALL.
           CALL "dump" USING G-3
           END-CALL.
           CALL "dump" USING G-4
           END-CALL.
           CALL "dump" USING G-5
           END-CALL.
           CALL "dump" USING G-6
           END-CALL.
           CALL "dump" USING G-7
           END-CALL.
           CALL "dump" USING G-8
           END-CALL.
           CALL "dump" USING G-9
           END-CALL.
           CALL "dump" USING G-10
           END-CALL.
           CALL "dump" USING G-11
           END-CALL.
           CALL "dump" USING G-12
           END-CALL.
           CALL "dump" USING G-13
           END-CALL.
           CALL "dump" USING G-14
           END-CALL.
           CALL "dump" USING G-15
           END-CALL.
           CALL "dump" USING G-16
           END-CALL.
           CALL "dump" USING G-17
           END-CALL.
           CALL "dump" USING G-18
           END-CALL.
           CALL "dump" USING G-S1
           END-CALL.
           CALL "dump" USING G-S2
           END-CALL.
           CALL "dump" USING G-S3
           END-CALL.
           CALL "dump" USING G-S4
           END-CALL.
           CALL "dump" USING G-S5
           END-CALL.
           CALL "dump" USING G-S6
           END-CALL.
           CALL "dump" USING G-S7
           END-CALL.
           CALL "dump" USING G-S8
           END-CALL.
           CALL "dump" USING G-S9
           END-CALL.
           CALL "dump" USING G-S10
           END-CALL.
           CALL "dump" USING G-S11
           END-CALL.
           CALL "dump" USING G-S12
           END-CALL.
           CALL "dump" USING G-S13
           END-CALL.
           CALL "dump" USING G-S14
           END-CALL.
           CALL "dump" USING G-S15
           END-CALL.
           CALL "dump" USING G-S16
           END-CALL.
           CALL "dump" USING G-S17
           END-CALL.
           CALL "dump" USING G-S18
           END-CALL.
           INITIALIZE X-1.
           CALL "dump" USING G-1
           END-CALL.
           INITIALIZE X-2.
           CALL "dump" USING G-2
           END-CALL.
           INITIALIZE X-3.
           CALL "dump" USING G-3
           END-CALL.
           INITIALIZE X-4.
           CALL "dump" USING G-4
           END-CALL.
           INITIALIZE X-5.
           CALL "dump" USING G-5
           END-CALL.
           INITIALIZE X-6.
           CALL "dump" USING G-6
           END-CALL.
           INITIALIZE X-7.
           CALL "dump" USING G-7
           END-CALL.
           INITIALIZE X-8.
           CALL "dump" USING G-8
           END-CALL.
           INITIALIZE X-9.
           CALL "dump" USING G-9
           END-CALL.
           INITIALIZE X-10.
           CALL "dump" USING G-10
           END-CALL.
           INITIALIZE X-11.
           CALL "dump" USING G-11
           END-CALL.
           INITIALIZE X-12.
           CALL "dump" USING G-12
           END-CALL.
           INITIALIZE X-13.
           CALL "dump" USING G-13
           END-CALL.
           INITIALIZE X-14.
           CALL "dump" USING G-14
           END-CALL.
           INITIALIZE X-15.
           CALL "dump" USING G-15
           END-CALL.
           INITIALIZE X-16.
           CALL "dump" USING G-16
           END-CALL.
           INITIALIZE X-17.
           CALL "dump" USING G-17
           END-CALL.
           INITIALIZE X-18.
           CALL "dump" USING G-18
           END-CALL.
           INITIALIZE X-S1.
           CALL "dump" USING G-S1
           END-CALL.
           INITIALIZE X-S2.
           CALL "dump" USING G-S2
           END-CALL.
           INITIALIZE X-S3.
           CALL "dump" USING G-S3
           END-CALL.
           INITIALIZE X-S4.
           CALL "dump" USING G-S4
           END-CALL.
           INITIALIZE X-S5.
           CALL "dump" USING G-S5
           END-CALL.
           INITIALIZE X-S6.
           CALL "dump" USING G-S6
           END-CALL.
           INITIALIZE X-S7.
           CALL "dump" USING G-S7
           END-CALL.
           INITIALIZE X-S8.
           CALL "dump" USING G-S8
           END-CALL.
           INITIALIZE X-S9.
           CALL "dump" USING G-S9
           END-CALL.
           INITIALIZE X-S10.
           CALL "dump" USING G-S10
           END-CALL.
           INITIALIZE X-S11.
           CALL "dump" USING G-S11
           END-CALL.
           INITIALIZE X-S12.
           CALL "dump" USING G-S12
           END-CALL.
           INITIALIZE X-S13.
           CALL "dump" USING G-S13
           END-CALL.
           INITIALIZE X-S14.
           CALL "dump" USING G-S14
           END-CALL.
           INITIALIZE X-S15.
           CALL "dump" USING G-S15
           END-CALL.
           INITIALIZE X-S16.
           CALL "dump" USING G-S16
           END-CALL.
           INITIALIZE X-S17.
           CALL "dump" USING G-S17
           END-CALL.
           INITIALIZE X-S18.
           CALL "dump" USING G-S18
           END-CALL.
           MOVE ZERO TO X-1.
           CALL "dump" USING G-1
           END-CALL.
           MOVE ZERO TO X-2.
           CALL "dump" USING G-2
           END-CALL.
           MOVE ZERO TO X-3.
           CALL "dump" USING G-3
           END-CALL.
           MOVE ZERO TO X-4.
           CALL "dump" USING G-4
           END-CALL.
           MOVE ZERO TO X-5.
           CALL "dump" USING G-5
           END-CALL.
           MOVE ZERO TO X-6.
           CALL "dump" USING G-6
           END-CALL.
           MOVE ZERO TO X-7.
           CALL "dump" USING G-7
           END-CALL.
           MOVE ZERO TO X-8.
           CALL "dump" USING G-8
           END-CALL.
           MOVE ZERO TO X-9.
           CALL "dump" USING G-9
           END-CALL.
           MOVE ZERO TO X-10.
           CALL "dump" USING G-10
           END-CALL.
           MOVE ZERO TO X-11.
           CALL "dump" USING G-11
           END-CALL.
           MOVE ZERO TO X-12.
           CALL "dump" USING G-12
           END-CALL.
           MOVE ZERO TO X-13.
           CALL "dump" USING G-13
           END-CALL.
           MOVE ZERO TO X-14.
           CALL "dump" USING G-14
           END-CALL.
           MOVE ZERO TO X-15.
           CALL "dump" USING G-15
           END-CALL.
           MOVE ZERO TO X-16.
           CALL "dump" USING G-16
           END-CALL.
           MOVE ZERO TO X-17.
           CALL "dump" USING G-17
           END-CALL.
           MOVE ZERO TO X-18.
           CALL "dump" USING G-18
           END-CALL.
           MOVE ZERO TO X-S1.
           CALL "dump" USING G-S1
           END-CALL.
           MOVE ZERO TO X-S2.
           CALL "dump" USING G-S2
           END-CALL.
           MOVE ZERO TO X-S3.
           CALL "dump" USING G-S3
           END-CALL.
           MOVE ZERO TO X-S4.
           CALL "dump" USING G-S4
           END-CALL.
           MOVE ZERO TO X-S5.
           CALL "dump" USING G-S5
           END-CALL.
           MOVE ZERO TO X-S6.
           CALL "dump" USING G-S6
           END-CALL.
           MOVE ZERO TO X-S7.
           CALL "dump" USING G-S7
           END-CALL.
           MOVE ZERO TO X-S8.
           CALL "dump" USING G-S8
           END-CALL.
           MOVE ZERO TO X-S9.
           CALL "dump" USING G-S9
           END-CALL.
           MOVE ZERO TO X-S10.
           CALL "dump" USING G-S10
           END-CALL.
           MOVE ZERO TO X-S11.
           CALL "dump" USING G-S11
           END-CALL.
           MOVE ZERO TO X-S12.
           CALL "dump" USING G-S12
           END-CALL.
           MOVE ZERO TO X-S13.
           CALL "dump" USING G-S13
           END-CALL.
           MOVE ZERO TO X-S14.
           CALL "dump" USING G-S14
           END-CALL.
           MOVE ZERO TO X-S15.
           CALL "dump" USING G-S15
           END-CALL.
           MOVE ZERO TO X-S16.
           CALL "dump" USING G-S16
           END-CALL.
           MOVE ZERO TO X-S17.
           CALL "dump" USING G-S17
           END-CALL.
           MOVE ZERO TO X-S18.
           CALL "dump" USING G-S18
           END-CALL.
           STOP RUN.
           END PROGRAM prog.
        IDENTIFICATION   DIVISION.
        PROGRAM-ID.      dump.
        DATA             DIVISION.
        WORKING-STORAGE SECTION.
        01      HEXCHARS.
          02    HEXCHART PIC X(16) VALUE "0123456789abcdef".
          02    HEXCHAR  REDEFINES HEXCHART PIC X OCCURS 16.
        01      BYTE-TO-DUMP PIC X(1).
        01      FILLER.
          02    DUMPER1 PIC 9999 COMP-5.
          02    DUMPER2 REDEFINES DUMPER1 PIC X(1).
        01      THE-BYTE PIC 99.
        01      LADVANCE PIC 9.
        LINKAGE SECTION.
        01 G-VAL PIC X(20).
        01 G-PTR REDEFINES G-VAL USAGE POINTER.
        PROCEDURE DIVISION USING G-VAL.
        MOVE 1 TO THE-BYTE
        MOVE 0 TO LADVANCE
        PERFORM UNTIL THE-BYTE GREATER THAN 10
            MOVE G-VAL(THE-BYTE:1) TO BYTE-TO-DUMP
            IF THE-BYTE EQUAL TO 10 MOVE 1 TO LADVANCE END-IF
            PERFORM DUMP-BYTE
            ADD 1 TO THE-BYTE
            END-PERFORM.
        GOBACK.
        DUMP-BYTE.
            MOVE ZERO TO DUMPER1
            MOVE BYTE-TO-DUMP TO DUMPER2
            DIVIDE DUMPER1 BY 16 GIVING DUMPER1
            ADD 1 TO DUMPER1
            DISPLAY HEXCHAR(DUMPER1) NO ADVANCING.
            MOVE ZERO TO DUMPER1
            MOVE BYTE-TO-DUMP TO DUMPER2
            MOVE FUNCTION MOD(DUMPER1 16) TO DUMPER1
            ADD 1 TO DUMPER1
            IF LADVANCE EQUAL TO 1 THEN
                DISPLAY HEXCHAR(DUMPER1)
            ELSE
                DISPLAY HEXCHAR(DUMPER1) NO ADVANCING
            END-IF.
        END PROGRAM dump.

