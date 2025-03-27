       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_DIVIDE_FORMAT_1.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-div1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 1.1 .
        01 S2 PIC 999V99 COMP           VALUE 1.1 .
        01 S3 PIC 999V99 COMP-3         VALUE 1.1 .
        01 S4 PIC 999V99 COMP-5         VALUE 1.1 .
        01 S5            FLOAT-SHORT    VALUE 1.1 .
        01 S6            FLOAT-LONG     VALUE 1.1 .
        01 S7            FLOAT-EXTENDED VALUE 1.1 .
        01 D1 PIC 999V99 DISPLAY        VALUE 611.05.
        01 D2 PIC 999V99 COMP           VALUE 611.05.
        01 D3 PIC 999V99 COMP-3         VALUE 611.05.
        01 D4 PIC 999V99 COMP-5         VALUE 611.05.
        01 D5            FLOAT-SHORT    VALUE 611.05.
        01 D6            FLOAT-LONG     VALUE 611.05.
        01 D7            FLOAT-EXTENDED VALUE 611.05.
        PROCEDURE DIVISION.
            DIVIDE S1 INTO D1
            DIVIDE S2 INTO D2
            DIVIDE S3 INTO D3
            DIVIDE S4 INTO D4
            DIVIDE S5 INTO D5
            DIVIDE S6 INTO D6
            DIVIDE S7 INTO D7
            PERFORM DISPLAY-D.
            DIVIDE S1 INTO D2
            DIVIDE S2 INTO D3
            DIVIDE S3 INTO D4
            DIVIDE S4 INTO D5
            DIVIDE S5 INTO D6
            DIVIDE S6 INTO D7
            DIVIDE S7 INTO D1
            PERFORM DISPLAY-D.
            DIVIDE S1 INTO D3
            DIVIDE S2 INTO D4
            DIVIDE S3 INTO D5
            DIVIDE S4 INTO D6
            DIVIDE S5 INTO D7
            DIVIDE S6 INTO D1
            DIVIDE S7 INTO D2
            PERFORM DISPLAY-D.
            DIVIDE S1 INTO D4
            DIVIDE S2 INTO D5
            DIVIDE S3 INTO D6
            DIVIDE S4 INTO D7
            DIVIDE S5 INTO D1
            DIVIDE S6 INTO D2
            DIVIDE S7 INTO D3
            PERFORM DISPLAY-D.
            DIVIDE S1 INTO D5
            DIVIDE S2 INTO D6
            DIVIDE S3 INTO D7
            DIVIDE S4 INTO D1
            DIVIDE S5 INTO D2
            DIVIDE S6 INTO D3
            DIVIDE S7 INTO D4
            PERFORM DISPLAY-D.
            DIVIDE S1 INTO D6
            DIVIDE S2 INTO D7
            DIVIDE S3 INTO D1
            DIVIDE S4 INTO D2
            DIVIDE S5 INTO D3
            DIVIDE S6 INTO D4
            DIVIDE S7 INTO D5
            PERFORM DISPLAY-D.
            DIVIDE S1 INTO D7
            DIVIDE S2 INTO D1
            DIVIDE S3 INTO D2
            DIVIDE S4 INTO D3
            DIVIDE S5 INTO D4
            DIVIDE S6 INTO D5
            DIVIDE S7 INTO D6
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
        END PROGRAM float-div1.

