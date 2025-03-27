       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_ADD_FORMAT_2.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-add2.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 123.45 .
        01 S2 PIC 999V99 COMP           VALUE 123.45 .
        01 S3 PIC 999V99 COMP-3         VALUE 123.45 .
        01 S4 PIC 999V99 COMP-5         VALUE 123.45 .
        01 S5            FLOAT-SHORT    VALUE 123.45 .
        01 S6            FLOAT-LONG     VALUE 123.45 .
        01 S7            FLOAT-EXTENDED VALUE 123.45 .
        01 D1 PIC 999V99 DISPLAY        VALUE 543.21 .
        01 D2 PIC 999V99 COMP           VALUE 543.21 .
        01 D3 PIC 999V99 COMP-3         VALUE 543.21 .
        01 D4 PIC 999V99 COMP-5         VALUE 543.21 .
        01 D5            FLOAT-SHORT    VALUE 543.21 .
        01 D6            FLOAT-LONG     VALUE 543.21 .
        01 D7            FLOAT-EXTENDED VALUE 543.21 .
        01 X1 PIC 999V99 DISPLAY        .
        01 X2 PIC 999V99 COMP           .
        01 X3 PIC 999V99 COMP-3         .
        01 X4 PIC 999V99 COMP-5         .
        01 X5            FLOAT-SHORT    .
        01 X6            FLOAT-LONG     .
        01 X7            FLOAT-EXTENDED .
        PROCEDURE DIVISION.
            ADD S1 TO D1        GIVING X1
            ADD S2 TO D2        GIVING X2
            ADD S3 TO D3        GIVING X3
            ADD S4 TO D4        GIVING X4
            ADD S5 TO D5        GIVING X5
            ADD S6 TO D6        GIVING X6
            ADD S7 TO D7        GIVING X7
            PERFORM DISPLAY-X.
            ADD S2 TO D1        GIVING X1
            ADD S3 TO D2        GIVING X2
            ADD S4 TO D3        GIVING X3
            ADD S5 TO D4        GIVING X4
            ADD S6 TO D5        GIVING X5
            ADD S7 TO D6        GIVING X6
            ADD S1 TO D7        GIVING X7
            PERFORM DISPLAY-X.
            ADD S3 TO D1        GIVING X1
            ADD S4 TO D2        GIVING X2
            ADD S5 TO D3        GIVING X3
            ADD S6 TO D4        GIVING X4
            ADD S7 TO D5        GIVING X5
            ADD S1 TO D6        GIVING X6
            ADD S2 TO D7        GIVING X7
            PERFORM DISPLAY-X.
            ADD S4 TO D1        GIVING X1
            ADD S5 TO D2        GIVING X2
            ADD S6 TO D3        GIVING X3
            ADD S7 TO D4        GIVING X4
            ADD S1 TO D5        GIVING X5
            ADD S2 TO D6        GIVING X6
            ADD S3 TO D7        GIVING X7
            PERFORM DISPLAY-X.
            ADD S5 TO D1        GIVING X1
            ADD S6 TO D2        GIVING X2
            ADD S7 TO D3        GIVING X3
            ADD S1 TO D4        GIVING X4
            ADD S2 TO D5        GIVING X5
            ADD S3 TO D6        GIVING X6
            ADD S4 TO D7        GIVING X7
            PERFORM DISPLAY-X.
            ADD S6 TO D1        GIVING X1
            ADD S7 TO D2        GIVING X2
            ADD S1 TO D3        GIVING X3
            ADD S2 TO D4        GIVING X4
            ADD S3 TO D5        GIVING X5
            ADD S4 TO D6        GIVING X6
            ADD S5 TO D7        GIVING X7
            PERFORM DISPLAY-X.
            ADD S7 TO D1        GIVING X1
            ADD S1 TO D2        GIVING X2
            ADD S2 TO D3        GIVING X3
            ADD S3 TO D4        GIVING X4
            ADD S4 TO D5        GIVING X5
            ADD S5 TO D6        GIVING X6
            ADD S6 TO D7        GIVING X7
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
        END PROGRAM float-add2.

