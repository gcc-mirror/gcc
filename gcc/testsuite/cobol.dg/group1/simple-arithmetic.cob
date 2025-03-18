*> { dg-do run }
*> { dg-output {Numeric Display arithmetic(\n|\r\n|\r)} }
*> { dg-output {Num1 is \+5; Num2 is \+4(\n|\r\n|\r)} }
*> { dg-output {Product should be    \+20, is = \+20(\n|\r\n|\r)} }
*> { dg-output {Sum should be        \+09, is = \+09(\n|\r\n|\r)} }
*> { dg-output {Difference should be \-01, is = \-01(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {COMP\-5 Arithmetic(\n|\r\n|\r)} }
*> { dg-output {Num1_5 is \+0000000000005; Num2_5 is \+0000000000004(\n|\r\n|\r)} }
*> { dg-output {Product should be    \+0000000000020, is = \+0000000000020(\n|\r\n|\r)} }
*> { dg-output {Sum should be        \+0000000000009, is = \+0000000000009(\n|\r\n|\r)} }
*> { dg-output {Difference should be \-0000000000001, is = \-0000000000001(\n|\r\n|\r)} }
*> { dg-output { (\n|\r\n|\r)} }
*> { dg-output {COMP\-3 Arithmetic(\n|\r\n|\r)} }
*> { dg-output {Num1_3 is \+0000000000005; Num2_3 is \+0000000000004(\n|\r\n|\r)} }
*> { dg-output {Product should be    \+0000000000020, is = \+0000000000020(\n|\r\n|\r)} }
*> { dg-output {Sum should be        \+0000000000009, is = \+0000000000009(\n|\r\n|\r)} }
*> { dg-output {Difference should be \-0000000000001, is = \-0000000000001(\n|\r\n|\r)} }
*> { dg-output { } }
        IDENTIFICATION DIVISION.
        PROGRAM-ID.  math.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  Num1         PIC S9  VALUE 5.
        01  Num2         PIC S9  VALUE 4.
        01  Result       PIC S99 VALUE ZEROS.
        01  Num1_5       PIC S9999999999999 COMP-5 VALUE 5.
        01  Num2_5       PIC S9999999999999 COMP-5 VALUE 4.
        01  Result_5     PIC S9999999999999 COMP-5 VALUE ZEROS.
        01  Num1_3       PIC S9999999999999 COMP-3 VALUE 5.
        01  Num2_3       PIC S9999999999999 COMP-3 VALUE 4.
        01  Result_3     PIC S9999999999999 COMP-3 VALUE ZEROS.
        PROCEDURE DIVISION.
            DISPLAY "Numeric Display arithmetic"
            DISPLAY "Num1 is " Num1 "; Num2 is " Num2
            MULTIPLY Num1 BY Num2 GIVING Result
            DISPLAY "Product should be    +20, is = ", Result
            ADD Num1 TO Num2 GIVING Result
            DISPLAY "Sum should be        +09, is = ", Result
            SUBTRACT Num1 FROM Num2 GIVING Result
            DISPLAY "Difference should be -01, is = ", Result
            DISPLAY " "
            DISPLAY "COMP-5 Arithmetic"
            DISPLAY "Num1_5 is " Num1_5 "; Num2_5 is " Num2_5
            MULTIPLY Num1_5 BY Num2_5 GIVING Result_5
            DISPLAY "Product should be    +0000000000020, is = ", Result_5
            ADD Num1_5 TO Num2_5 GIVING Result_5
            DISPLAY "Sum should be        +0000000000009, is = ", Result_5
            SUBTRACT Num1_5 FROM Num2_5 GIVING Result_5
            DISPLAY "Difference should be -0000000000001, is = ", Result_5
            DISPLAY " "
            DISPLAY "COMP-3 Arithmetic"
            DISPLAY "Num1_3 is " Num1_3 "; Num2_3 is " Num2_3
            MULTIPLY Num1_3 BY Num2_3 GIVING Result_3
            DISPLAY "Product should be    +0000000000020, is = ", Result_3
            ADD Num1_3 TO Num2_3 GIVING Result_3
            DISPLAY "Sum should be        +0000000000009, is = ", Result_3
            SUBTRACT Num1_3 FROM Num2_3 GIVING Result_3
            DISPLAY "Difference should be -0000000000001, is = ", Result_3
            DISPLAY " "
            STOP RUN.
