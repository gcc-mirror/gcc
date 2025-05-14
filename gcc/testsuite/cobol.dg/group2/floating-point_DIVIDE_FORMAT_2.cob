       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_DIVIDE_FORMAT_2.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-DIVIDE2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 123.21 .
        01 S2 PIC 999V99 COMP           VALUE 123.21 .
        01 S3 PIC 999V99 COMP-3         VALUE 123.21 .
        01 S4 PIC 999V99 COMP-5         VALUE 123.21 .
        01 S5            FLOAT-SHORT    VALUE 123.21 .
        01 S6            FLOAT-LONG     VALUE 123.21 .
        01 S7            FLOAT-EXTENDED VALUE 123.21 .
        01 D1 PIC 999V99 DISPLAY        VALUE 111.00 .
        01 D2 PIC 999V99 COMP           VALUE 111.00 .
        01 D3 PIC 999V99 COMP-3         VALUE 111.00 .
        01 D4 PIC 999V99 COMP-5         VALUE 111.00 .
        01 D5            FLOAT-SHORT    VALUE 111.00 .
        01 D6            FLOAT-LONG     VALUE 111.00 .
        01 D7            FLOAT-EXTENDED VALUE 111.00 .
        01 X1 PIC 999V99 DISPLAY        .
        01 X2 PIC 999V99 COMP           .
        01 X3 PIC 999V99 COMP-3         .
        01 X4 PIC 999V99 COMP-5         .
        01 X5            FLOAT-SHORT    .
        01 X6            FLOAT-LONG     .
        01 X7            FLOAT-EXTENDED .
        PROCEDURE DIVISION.
            DIVIDE S1 BY D1        GIVING X1
            DIVIDE S2 BY D2        GIVING X2
            DIVIDE S3 BY D3        GIVING X3
            DIVIDE S4 BY D4        GIVING X4
            DIVIDE S5 BY D5        GIVING X5
            DIVIDE S6 BY D6        GIVING X6
            DIVIDE S7 BY D7        GIVING X7
            PERFORM DISPLAY-X.
            DIVIDE S2 BY D1        GIVING X1
            DIVIDE S3 BY D2        GIVING X2
            DIVIDE S4 BY D3        GIVING X3
            DIVIDE S5 BY D4        GIVING X4
            DIVIDE S6 BY D5        GIVING X5
            DIVIDE S7 BY D6        GIVING X6
            DIVIDE S1 BY D7        GIVING X7
            PERFORM DISPLAY-X.
            DIVIDE S3 BY D1        GIVING X1
            DIVIDE S4 BY D2        GIVING X2
            DIVIDE S5 BY D3        GIVING X3
            DIVIDE S6 BY D4        GIVING X4
            DIVIDE S7 BY D5        GIVING X5
            DIVIDE S1 BY D6        GIVING X6
            DIVIDE S2 BY D7        GIVING X7
            PERFORM DISPLAY-X.
            DIVIDE S4 BY D1        GIVING X1
            DIVIDE S5 BY D2        GIVING X2
            DIVIDE S6 BY D3        GIVING X3
            DIVIDE S7 BY D4        GIVING X4
            DIVIDE S1 BY D5        GIVING X5
            DIVIDE S2 BY D6        GIVING X6
            DIVIDE S3 BY D7        GIVING X7
            PERFORM DISPLAY-X.
            DIVIDE S5 BY D1        GIVING X1
            DIVIDE S6 BY D2        GIVING X2
            DIVIDE S7 BY D3        GIVING X3
            DIVIDE S1 BY D4        GIVING X4
            DIVIDE S2 BY D5        GIVING X5
            DIVIDE S3 BY D6        GIVING X6
            DIVIDE S4 BY D7        GIVING X7
            PERFORM DISPLAY-X.
            DIVIDE S6 BY D1        GIVING X1
            DIVIDE S7 BY D2        GIVING X2
            DIVIDE S1 BY D3        GIVING X3
            DIVIDE S2 BY D4        GIVING X4
            DIVIDE S3 BY D5        GIVING X5
            DIVIDE S4 BY D6        GIVING X6
            DIVIDE S5 BY D7        GIVING X7
            PERFORM DISPLAY-X.
            DIVIDE S7 BY D1        GIVING X1
            DIVIDE S1 BY D2        GIVING X2
            DIVIDE S2 BY D3        GIVING X3
            DIVIDE S3 BY D4        GIVING X4
            DIVIDE S4 BY D5        GIVING X5
            DIVIDE S5 BY D6        GIVING X6
            DIVIDE S6 BY D7        GIVING X7
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
        END PROGRAM float-DIVIDE2.

