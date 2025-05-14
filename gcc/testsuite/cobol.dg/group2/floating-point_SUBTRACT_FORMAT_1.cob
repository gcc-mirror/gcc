       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_SUBTRACT_FORMAT_1.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-sub1.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 111.11 .
        01 S2 PIC 999V99 COMP           VALUE 111.11 .
        01 S3 PIC 999V99 COMP-3         VALUE 111.11 .
        01 S4 PIC 999V99 COMP-5         VALUE 111.11 .
        01 S5            FLOAT-SHORT    VALUE 111.11 .
        01 S6            FLOAT-LONG     VALUE 111.11 .
        01 S7            FLOAT-EXTENDED VALUE 111.11 .
        01 D1 PIC 999V99 DISPLAY        VALUE 666.66.
        01 D2 PIC 999V99 COMP           VALUE 666.66.
        01 D3 PIC 999V99 COMP-3         VALUE 666.66.
        01 D4 PIC 999V99 COMP-5         VALUE 666.66.
        01 D5            FLOAT-SHORT    VALUE 666.66.
        01 D6            FLOAT-LONG     VALUE 666.66.
        01 D7            FLOAT-EXTENDED VALUE 666.66.
        PROCEDURE DIVISION.
            SUBTRACT S1 FROM D1
            SUBTRACT S1 FROM D2
            SUBTRACT S1 FROM D3
            SUBTRACT S1 FROM D4
            SUBTRACT S1 FROM D5
            SUBTRACT S1 FROM D6
            SUBTRACT S1 FROM D7
            PERFORM DISPLAY-D.
            SUBTRACT S2 FROM D2
            SUBTRACT S2 FROM D3
            SUBTRACT S2 FROM D4
            SUBTRACT S2 FROM D5
            SUBTRACT S2 FROM D6
            SUBTRACT S2 FROM D7
            SUBTRACT S2 FROM D1
            PERFORM DISPLAY-D.
            SUBTRACT S3 FROM D3
            SUBTRACT S3 FROM D4
            SUBTRACT S3 FROM D5
            SUBTRACT S3 FROM D6
            SUBTRACT S3 FROM D7
            SUBTRACT S3 FROM D1
            SUBTRACT S3 FROM D2
            PERFORM DISPLAY-D.
            SUBTRACT S4 FROM D4
            SUBTRACT S4 FROM D5
            SUBTRACT S4 FROM D6
            SUBTRACT S4 FROM D7
            SUBTRACT S4 FROM D1
            SUBTRACT S4 FROM D2
            SUBTRACT S4 FROM D3
            PERFORM DISPLAY-D.
            SUBTRACT S5 FROM D5
            SUBTRACT S5 FROM D6
            SUBTRACT S5 FROM D7
            SUBTRACT S5 FROM D1
            SUBTRACT S5 FROM D2
            SUBTRACT S5 FROM D3
            SUBTRACT S5 FROM D4
            PERFORM DISPLAY-D.
            SUBTRACT S6 FROM D6
            SUBTRACT S6 FROM D7
            SUBTRACT S6 FROM D1
            SUBTRACT S6 FROM D2
            SUBTRACT S6 FROM D3
            SUBTRACT S6 FROM D4
            SUBTRACT S6 FROM D5
            PERFORM DISPLAY-D.
            SUBTRACT S7 FROM D7
            SUBTRACT S7 FROM D1
            SUBTRACT S7 FROM D2
            SUBTRACT S7 FROM D3
            SUBTRACT S7 FROM D4
            SUBTRACT S7 FROM D5
            SUBTRACT S7 FROM D6
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
        END PROGRAM float-sub1.

