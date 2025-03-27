       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_MULTIPLY_FORMAT_1.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-mult1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 1.2 .
        01 S2 PIC 999V99 COMP           VALUE 1.2 .
        01 S3 PIC 999V99 COMP-3         VALUE 1.2 .
        01 S4 PIC 999V99 COMP-5         VALUE 1.2 .
        01 S5            FLOAT-SHORT    VALUE 1.2 .
        01 S6            FLOAT-LONG     VALUE 1.2 .
        01 S7            FLOAT-EXTENDED VALUE 1.2 .
        01 D1 PIC 999V99 DISPLAY        VALUE 1.1.
        01 D2 PIC 999V99 COMP           VALUE 1.1.
        01 D3 PIC 999V99 COMP-3         VALUE 1.1.
        01 D4 PIC 999V99 COMP-5         VALUE 1.1.
        01 D5            FLOAT-SHORT    VALUE 1.1.
        01 D6            FLOAT-LONG     VALUE 1.1.
        01 D7            FLOAT-EXTENDED VALUE 1.1.
        PROCEDURE DIVISION.
            MULTIPLY S1 BY D1
            MULTIPLY S2 BY D2
            MULTIPLY S3 BY D3
            MULTIPLY S4 BY D4
            MULTIPLY S5 BY D5
            MULTIPLY S6 BY D6
            MULTIPLY S7 BY D7
            PERFORM DISPLAY-D.
            MULTIPLY S1 BY D2
            MULTIPLY S2 BY D3
            MULTIPLY S3 BY D4
            MULTIPLY S4 BY D5
            MULTIPLY S5 BY D6
            MULTIPLY S6 BY D7
            MULTIPLY S7 BY D1
            PERFORM DISPLAY-D.
            MULTIPLY S1 BY D3
            MULTIPLY S2 BY D4
            MULTIPLY S3 BY D5
            MULTIPLY S4 BY D6
            MULTIPLY S5 BY D7
            MULTIPLY S6 BY D1
            MULTIPLY S7 BY D2
            PERFORM DISPLAY-D.
            MULTIPLY S1 BY D4
            MULTIPLY S2 BY D5
            MULTIPLY S3 BY D6
            MULTIPLY S4 BY D7
            MULTIPLY S5 BY D1
            MULTIPLY S6 BY D2
            MULTIPLY S7 BY D3
            PERFORM DISPLAY-D.
            MULTIPLY S1 BY D5
            MULTIPLY S2 BY D6
            MULTIPLY S3 BY D7
            MULTIPLY S4 BY D1
            MULTIPLY S5 BY D2
            MULTIPLY S6 BY D3
            MULTIPLY S7 BY D4
            PERFORM DISPLAY-D.
            MULTIPLY S1 BY D6
            MULTIPLY S2 BY D7
            MULTIPLY S3 BY D1
            MULTIPLY S4 BY D2
            MULTIPLY S5 BY D3
            MULTIPLY S6 BY D4
            MULTIPLY S7 BY D5
            PERFORM DISPLAY-D.
            MULTIPLY S1 BY D7
            MULTIPLY S2 BY D1
            MULTIPLY S3 BY D2
            MULTIPLY S4 BY D3
            MULTIPLY S5 BY D4
            MULTIPLY S6 BY D5
            MULTIPLY S7 BY D6
            PERFORM DISPLAY-D.
            GOBACK.
        DISPLAY-D.
            DISPLAY D1 SPACE
                    D2 SPACE
                    D3 SPACE
                    D4 SPACE
                    D5 SPACE
                    D6 SPACE
                    D7 .
            INITIALIZE D1 D2 D3 D4 D5 D6 D7 ALL VALUE.
        END PROGRAM float-mult1.

