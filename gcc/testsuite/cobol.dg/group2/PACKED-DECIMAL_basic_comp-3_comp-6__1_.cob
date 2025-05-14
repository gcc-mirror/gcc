       *> { dg-do run }
       *> { dg-options "-dialect mf" }
       *> { dg-output-file "group2/PACKED-DECIMAL_basic_comp-3_comp-6__1_.out" }

        IDENTIFICATION DIVISION.
        PROGRAM-ID. prog.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  x1 PIC 9 COMP-3.
        01  x2 PIC 99 COMP-3.
        01  x3 PIC 999 COMP-3.
        01  x4 PIC 9999 COMP-3.
        01  x5 PIC 99999 COMP-3.
        01  x6 PIC 999999 COMP-3.
        01  y1 PIC 9 COMP-6.
        01  y2 PIC 99 COMP-6.
        01  y3 PIC 999 COMP-6.
        01  y4 PIC 9999 COMP-6.
        01  y5 PIC 99999 COMP-6.
        01  y6 PIC 999999 COMP-6.
        procedure division.
        display "check lengths of comp-3"
        display FUNCTION LENGTH(x1) " should be 1"
        display FUNCTION LENGTH(x2) " should be 2"
        display FUNCTION LENGTH(x3) " should be 2"
        display FUNCTION LENGTH(x4) " should be 3"
        display FUNCTION LENGTH(x5) " should be 3"
        display FUNCTION LENGTH(x6) " should be 4"
        display "check lengths of comp-6"       
        display FUNCTION LENGTH(y1) " should be 1"
        display FUNCTION LENGTH(y2) " should be 1"
        display FUNCTION LENGTH(y3) " should be 2"
        display FUNCTION LENGTH(y4) " should be 2"
        display FUNCTION LENGTH(y5) " should be 3"
        display FUNCTION LENGTH(y6) " should be 3"
        move 654321 to x1 x2 x3 x4 x5 x6 y1 y2 y3 y4 y5 y6
        display "results of MOVE TO COMP-3"
        display x1
        display x2
        display x3
        display x4
        display x5
        display x6
        display "results of MOVE TO COMP-6"
        display y1
        display y2
        display y3
        display y4
        display y5
        display y6
        goback.

