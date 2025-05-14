       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_SUBTRACT_FORMAT_2.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-SUBTRACT2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 123.45 .
        01 S2 PIC 999V99 COMP           VALUE 123.45 .
        01 S3 PIC 999V99 COMP-3         VALUE 123.45 .
        01 S4 PIC 999V99 COMP-5         VALUE 123.45 .
        01 S5            FLOAT-SHORT    VALUE 123.45 .
        01 S6            FLOAT-LONG     VALUE 123.45 .
        01 S7            FLOAT-EXTENDED VALUE 123.45 .
        01 D1 PIC 999V99 DISPLAY        VALUE 678.55 .
        01 D2 PIC 999V99 COMP           VALUE 678.55 .
        01 D3 PIC 999V99 COMP-3         VALUE 678.55 .
        01 D4 PIC 999V99 COMP-5         VALUE 678.55 .
        01 D5            FLOAT-SHORT    VALUE 678.55 .
        01 D6            FLOAT-LONG     VALUE 678.55 .
        01 D7            FLOAT-EXTENDED VALUE 678.55 .
        01 X1 PIC 999V99 DISPLAY        .
        01 X2 PIC 999V99 COMP           .
        01 X3 PIC 999V99 COMP-3         .
        01 X4 PIC 999V99 COMP-5         .
        01 X5            FLOAT-SHORT    .
        01 X6            FLOAT-LONG     .
        01 X7            FLOAT-EXTENDED .
        PROCEDURE DIVISION.
            SUBTRACT S1 FROM D1        GIVING X1
            SUBTRACT S2 FROM D2        GIVING X2
            SUBTRACT S3 FROM D3        GIVING X3
            SUBTRACT S4 FROM D4        GIVING X4
            SUBTRACT S5 FROM D5        GIVING X5
            SUBTRACT S6 FROM D6        GIVING X6
            SUBTRACT S7 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            SUBTRACT S2 FROM D1        GIVING X1
            SUBTRACT S3 FROM D2        GIVING X2
            SUBTRACT S4 FROM D3        GIVING X3
            SUBTRACT S5 FROM D4        GIVING X4
            SUBTRACT S6 FROM D5        GIVING X5
            SUBTRACT S7 FROM D6        GIVING X6
            SUBTRACT S1 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            SUBTRACT S3 FROM D1        GIVING X1
            SUBTRACT S4 FROM D2        GIVING X2
            SUBTRACT S5 FROM D3        GIVING X3
            SUBTRACT S6 FROM D4        GIVING X4
            SUBTRACT S7 FROM D5        GIVING X5
            SUBTRACT S1 FROM D6        GIVING X6
            SUBTRACT S2 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            SUBTRACT S4 FROM D1        GIVING X1
            SUBTRACT S5 FROM D2        GIVING X2
            SUBTRACT S6 FROM D3        GIVING X3
            SUBTRACT S7 FROM D4        GIVING X4
            SUBTRACT S1 FROM D5        GIVING X5
            SUBTRACT S2 FROM D6        GIVING X6
            SUBTRACT S3 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            SUBTRACT S5 FROM D1        GIVING X1
            SUBTRACT S6 FROM D2        GIVING X2
            SUBTRACT S7 FROM D3        GIVING X3
            SUBTRACT S1 FROM D4        GIVING X4
            SUBTRACT S2 FROM D5        GIVING X5
            SUBTRACT S3 FROM D6        GIVING X6
            SUBTRACT S4 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            SUBTRACT S6 FROM D1        GIVING X1
            SUBTRACT S7 FROM D2        GIVING X2
            SUBTRACT S1 FROM D3        GIVING X3
            SUBTRACT S2 FROM D4        GIVING X4
            SUBTRACT S3 FROM D5        GIVING X5
            SUBTRACT S4 FROM D6        GIVING X6
            SUBTRACT S5 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            SUBTRACT S7 FROM D1        GIVING X1
            SUBTRACT S1 FROM D2        GIVING X2
            SUBTRACT S2 FROM D3        GIVING X3
            SUBTRACT S3 FROM D4        GIVING X4
            SUBTRACT S4 FROM D5        GIVING X5
            SUBTRACT S5 FROM D6        GIVING X6
            SUBTRACT S6 FROM D7        GIVING X7
            PERFORM DISPLAY-X.
            GOBACK.
        DISPLAY-X.
            DISPLAY X1 SPACE
                    X2 SPACE
                    X3 SPACE
                    X4 SPACE
                    X5 SPACE
                    X6 SPACE
                    X7 .
        END PROGRAM float-SUBTRACT2.

