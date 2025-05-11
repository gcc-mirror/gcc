       *> { dg-do run }
       *> { dg-output-file "group2/338_Default_Arithmetic__1_.out" }

       IDENTIFICATION DIVISION.
       PROGRAM-ID. prog.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM-A   PIC 9(3) VALUE 399.
       01 NUM-B   PIC 9(3) VALUE 211.
       01 NUM-C   PIC 9(3)V99 VALUE 212.34.
       01 NUMV1   PIC 9(3)V9.
       01 PICX    PIC X VALUE 'A'.
       01 RSLT    PIC 9(3).
       01 RSLTV1  PIC 9(3).9.
       01 RSLTV2  PIC 9(3).99.
      *
       PROCEDURE DIVISION.
       MAIN.
           COMPUTE RSLT = NUM-A + 1.1.
           DISPLAY 'Simple Compute  RSLT IS ' RSLT
           COMPUTE RSLT = ((NUM-A / 100) - (NUM-B / 100)) * 100
           DISPLAY 'Single Variable RSLT IS ' RSLT
           COMPUTE RSLTV2, RSLT = ((NUM-A / 100) - (NUM-B / 100)) * 100
           DISPLAY 'Compute  RSLT    IS ' RSLT
           DISPLAY 'Compute  RSLTv99 IS ' RSLTV2
           COMPUTE RSLTV1, RSLT = ((NUM-A / 100) - (NUM-B / 100)) * 100
           DISPLAY 'Compute  RSLT    IS ' RSLT
           DISPLAY 'Compute  RSLTv9  IS ' RSLTV1
           MOVE 0 TO RSLT
           ADD NUM-C TO RSLT.
           DISPLAY 'Add      RSLT    IS ' RSLT.
           MOVE 0 TO RSLT
           ADD NUM-A NUM-C 10 TO RSLT.
           DISPLAY 'Add      RSLT    IS ' RSLT.
           SUBTRACT NUM-C FROM RSLT.
           DISPLAY 'Subtract RSLT    IS ' RSLT.
           SUBTRACT NUM-A -10 FROM RSLT.
           DISPLAY 'Subtract RSLT    IS ' RSLT.
           MOVE 0 TO RSLT
           ADD NUM-A NUM-C TO RSLT GIVING RSLTV1.
           DISPLAY 'Add      RSLTv9  IS ' RSLTV1
           MULTIPLY NUM-A BY NUM-C GIVING RSLT.
           DISPLAY 'Multiply RSLT    IS ' RSLT.
           MULTIPLY RSLT BY NUM-C.
           DISPLAY 'Multiply RSLT    IS ' RSLT.
           DIVIDE NUM-A BY 10 GIVING RSLT.
           DISPLAY 'Divide   RSLT    IS ' RSLT.
           DIVIDE RSLT BY 4 GIVING RSLTV1.
           DISPLAY 'Divide   RSLTv9  IS ' RSLTV1.
           DIVIDE RSLT BY 4 GIVING RSLT.
           DISPLAY 'Divide   RSLT    IS ' RSLT.

           COMPUTE RSLTV1, RSLT = ((NUM-A / 100) - (NUM-B / 100)) * 100
           DISPLAY 'Simple   RSLT    IS ' RSLT
                           ' RSLTv9  IS ' RSLTV1.

           COMPUTE RSLTV1, RSLT = ((NUM-A / (100.55 + -0.550))
                                -  (NUM-B / (10.11 * 10 - 1.1)))
                                  * (220 / 2.2)
           DISPLAY 'Complex  RSLT    IS ' RSLT
                           ' RSLTv9  IS ' RSLTV1.

           COMPUTE RSLTV1, RSLT = ((NUM-A / (101 - 1))
                                -  (NUM-B / (10 * 10))) * (200 / 2)
           DISPLAY 'Reduced  RSLT    IS ' RSLT
                           ' RSLTv9  IS ' RSLTV1.
           MOVE NUM-A TO NUMV1.
           IF ((NUMV1 / (101 - 1))
              -  (NUM-B / (10 * 10))) * (200 / 2) EQUAL 188
              DISPLAY "Not Using ARITHMETIC-OSVS"
           ELSE
              DISPLAY "Using ARITHMETIC-OSVS"
           END-IF.
           STOP RUN.

