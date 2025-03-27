       *> { dg-do run }
       *> { dg-output-file "group2/floating-point_literals.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. float-literal.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 D1 PIC 999V9999 DISPLAY      .
        01 D2 PIC 999V9999 COMP         .
        01 D3 PIC 999V9999 COMP-3       .
        01 D4 PIC 999V9999 COMP-5       .
        01 D5            FLOAT-SHORT    .
        01 D6            FLOAT-LONG     .
        01 D7            FLOAT-EXTENDED .
        PROCEDURE DIVISION.
            DISPLAY -555
            DISPLAY -555.55
            DISPLAY -555.55e206
            DISPLAY 555
            DISPLAY 555.55
            DISPLAY 555.55e206
            MOVE 333.33 TO D1
            MOVE 333.33 TO D2
            MOVE 333.33 TO D3
            MOVE 333.33 TO D4
            MOVE 333.33e20 TO D5
            MOVE 333.33e100 TO D6
            MOVE 333.33e200 TO D7
            PERFORM DISPLAY-D.
            ADD 222.22 TO D1
            ADD 222.22 TO D2
            ADD 222.22 TO D3
            ADD 222.22 TO D4
            ADD 222.22e20 TO D5
            ADD 222.22e100 TO D6
            ADD 222.22e200 TO D7
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
        END PROGRAM float-literal.

