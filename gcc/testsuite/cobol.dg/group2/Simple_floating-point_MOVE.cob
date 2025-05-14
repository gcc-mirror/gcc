       *> { dg-do run }
       *> { dg-output-file "group2/Simple_floating-point_MOVE.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-move.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 S1 PIC 999V99 DISPLAY        VALUE 123.45 .
        01 S2 PIC 999V99 COMP           VALUE 123.45 .
        01 S3 PIC 999V99 COMP-3         VALUE 123.45 .
        01 S4 PIC 999V99 COMP-5         VALUE 123.45 .
        01 S5            FLOAT-SHORT    VALUE 123.45 .
        01 S6            FLOAT-LONG     VALUE 123.45 .
        01 S7            FLOAT-EXTENDED VALUE 123.45 .
        01 D1 PIC 999V99 DISPLAY        .
        01 D2 PIC 999V99 COMP           .
        01 D3 PIC 999V99 COMP-3         .
        01 D4 PIC 999V99 COMP-5         .
        01 D5            FLOAT-SHORT    .
        01 D6            FLOAT-LONG     .
        01 D7            FLOAT-EXTENDED .
        PROCEDURE DIVISION.
            MOVE S1 TO D1 D2 D3 D4 D5 D6 D7
            PERFORM DISPLAY-D.
            MOVE S2 TO D1 D2 D3 D4 D5 D6 D7
            PERFORM DISPLAY-D.
            MOVE S3 TO D1 D2 D3 D4 D5 D6 D7
            PERFORM DISPLAY-D.
            MOVE S4 TO D1 D2 D3 D4 D5 D6 D7
            PERFORM DISPLAY-D.
            MOVE S5 TO D1 D2 D3 D4 D5 D6 D7
            PERFORM DISPLAY-D.
            MOVE S6 TO D1 D2 D3 D4 D5 D6 D7
            PERFORM DISPLAY-D.
            MOVE S7 TO D1 D2 D3 D4 D5 D6 D7
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
            MOVE 0 TO D1 D2 D3 D4 D5 D6 D7.
        END PROGRAM float-move.

