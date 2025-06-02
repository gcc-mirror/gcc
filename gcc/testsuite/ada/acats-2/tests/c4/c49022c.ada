-- C49022C.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT NAMED NUMBER DECLARATIONS (REAL) MAY USE EXPRESSIONS
-- WITH REALS.

-- BAW 29 SEPT 80
-- TBN 10/24/85     RENAMED FROM C4A011A.ADA. ADDED RELATIONAL
--                  OPERATORS AND NAMED NUMBERS.

WITH REPORT;
PROCEDURE C49022C IS

     USE REPORT;

     ADD1 : CONSTANT :=   2.5  +   1.5;
     ADD2 : CONSTANT :=   2.5  + (-1.5);
     ADD3 : CONSTANT := (-2.5) +   1.5;
     ADD4 : CONSTANT := (-2.5) + (-1.5);
     SUB1 : CONSTANT :=   2.5  -   1.5;
     SUB2 : CONSTANT :=   2.5  - (-1.5);
     SUB3 : CONSTANT := (-2.5) -   1.5;
     SUB4 : CONSTANT := (-2.5) - (-1.5);
     MUL1 : CONSTANT :=   2.5  *   1.5;
     MUL2 : CONSTANT :=   2.5  * (-1.5);
     MUL3 : CONSTANT := (-2.5) *   1.5;
     MUL4 : CONSTANT := (-2.5) * (-1.5);
     MLR1 : CONSTANT :=   2    *   1.5;
     MLR2 : CONSTANT := (-2)   *   1.5;
     MLR3 : CONSTANT :=   2    * (-1.5);
     MLR4 : CONSTANT := (-2)   * (-1.5);
     MLL1 : CONSTANT :=  1.5   *    2 ;
     MLL2 : CONSTANT :=  1.5   *  (-2);
     MLL3 : CONSTANT :=(-1.5)  *    2 ;
     MLL4 : CONSTANT :=(-1.5)  *  (-2);
     DIV1 : CONSTANT :=   3.75  /   2.5;
     DIV2 : CONSTANT :=   3.75  / (-2.5);
     DIV3 : CONSTANT := (-3.75) /   2.5;
     DIV4 : CONSTANT := (-3.75) / (-2.5);
     DVI1 : CONSTANT :=   3.0   /   2;
     DVI2 : CONSTANT := (-3.0)  /   2;
     DVI3 : CONSTANT :=   3.0   / (-2);
     DVI4 : CONSTANT := (-3.0)  / (-2);
     EXP1 : CONSTANT :=   2.0  **   1;
     EXP2 : CONSTANT :=   2.0  ** (-1);
     EXP3 : CONSTANT := (-2.0) **   1;
     EXP4 : CONSTANT := (-2.0) ** (-1);
     ABS1 : CONSTANT := ABS( - 3.75 );
     ABS2 : CONSTANT := ABS( + 3.75 );
     TOT1 : CONSTANT := ADD1 + SUB4 - MUL1 + DIV1 - EXP2 + ABS1;
     LES1 : CONSTANT := BOOLEAN'POS (1.5 < 2.0);
     LES2 : CONSTANT := BOOLEAN'POS (1.5 < (-2.0));
     LES3 : CONSTANT := BOOLEAN'POS ((-1.5) < (-2.0));
     LES4 : CONSTANT := BOOLEAN'POS (ADD2 < SUB1);
     GRE1 : CONSTANT := BOOLEAN'POS (2.0 > 1.5);
     GRE2 : CONSTANT := BOOLEAN'POS ((-2.0) > 1.5);
     GRE3 : CONSTANT := BOOLEAN'POS ((-2.0) > (-1.5));
     GRE4 : CONSTANT := BOOLEAN'POS (ADD1 > SUB1);
     LEQ1 : CONSTANT := BOOLEAN'POS (1.5 <= 2.0);
     LEQ2 : CONSTANT := BOOLEAN'POS (1.5 <= (-2.0));
     LEQ3 : CONSTANT := BOOLEAN'POS ((-1.5) <= (-2.0));
     LEQ4 : CONSTANT := BOOLEAN'POS (ADD2 <= SUB1);
     GEQ1 : CONSTANT := BOOLEAN'POS (2.0 >= 1.5);
     GEQ2 : CONSTANT := BOOLEAN'POS ((-2.0) >= 1.5);
     GEQ3 : CONSTANT := BOOLEAN'POS ((-2.0) >= (-1.5));
     GEQ4 : CONSTANT := BOOLEAN'POS (ADD1 >= SUB2);
     EQU1 : CONSTANT := BOOLEAN'POS (1.5 = 2.0);
     EQU2 : CONSTANT := BOOLEAN'POS ((-1.5) = 2.0);
     EQU3 : CONSTANT := BOOLEAN'POS ((-1.5) = (-1.5));
     EQU4 : CONSTANT := BOOLEAN'POS (ADD1 = SUB2);
     NEQ1 : CONSTANT := BOOLEAN'POS (1.5 /= 1.5);
     NEQ2 : CONSTANT := BOOLEAN'POS ((-1.5) /= 1.5);
     NEQ3 : CONSTANT := BOOLEAN'POS ((-1.5) /= (-2.0));
     NEQ4 : CONSTANT := BOOLEAN'POS (ADD1 /= SUB2);


BEGIN
     TEST("C49022C","CHECK THAT NAMED NUMBER DECLARATIONS (REAL) " &
                    "MAY USE EXPRESSIONS WITH REALS.");

     IF ADD1 /= 4.0 OR ADD2 /= 1.0 OR ADD3 /= -1.0 OR ADD4 /= -4.0 THEN
          FAILED("ERROR IN THE ADDING OPERATOR +");
     END IF;

     IF SUB1 /= 1.0 OR SUB2 /= 4.0 OR SUB3 /= -4.0 OR SUB4 /= -1.0 THEN
          FAILED("ERROR IN THE ADDING OPERATOR -");
     END IF;

     IF MUL1 /= 3.75 OR MUL2 /= -3.75 OR
        MUL3 /= -3.75 OR MUL4 /= 3.75 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR *");
     END IF;

     IF MLR1 /= 3.0 OR MLR2 /= -3.0 OR
        MLR3 /= -3.0 OR MLR4 /= 3.0 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR *");
     END IF;

     IF MLL1 /= 3.0 OR MLL2 /= -3.0 OR MLL3 /= -3.0 OR MLL4 /= 3.0 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR *");
     END IF;

     IF DIV1 /= 1.5 OR DIV2 /= -1.5 OR DIV3 /= -1.5 OR DIV4 /= 1.5 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR /");
     END IF;

     IF DVI1 /= 1.5 OR DVI2 /= -1.5 OR DVI3 /= -1.5 OR DVI4 /= 1.5 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR /");
     END IF;

     IF EXP1 /= 2.0 OR EXP2 /= 0.5 OR EXP3 /= -2.0 OR EXP4 /= -0.5 THEN
          FAILED("ERROR IN THE EXPONENTIATING OPERATOR");
     END IF;

     IF ABS1 /= 3.75 OR ABS2 /= 3.75 THEN
          FAILED("ERROR IN THE ABS OPERATOR");
     END IF;

     IF TOT1 /= 4.00 THEN
          FAILED("ERROR IN USE OF NAMED NUMBERS WITH OPERATORS");
     END IF;

     IF LES1 /= 1 OR LES2 /= 0 OR LES3 /= 0 OR LES4 /= 0 THEN
          FAILED("ERROR IN THE LESS THAN OPERATOR");
     END IF;

     IF GRE1 /= 1 OR GRE2 /= 0 OR GRE3 /= 0 OR GRE4 /= 1 THEN
          FAILED("ERROR IN THE GREATER THAN OPERATOR");
     END IF;

     IF LEQ1 /= 1 OR LEQ2 /= 0 OR LEQ3 /= 0 OR LEQ4 /= 1 THEN
          FAILED("ERROR IN THE LESS THAN EQUAL OPERATOR");
     END IF;

     IF GEQ1 /= 1 OR GEQ2 /= 0 OR GEQ3 /= 0 OR GEQ4 /= 1 THEN
          FAILED("ERROR IN THE GREATER THAN EQUAL OPERATOR");
     END IF;

     IF EQU1 /= 0 OR EQU2 /= 0 OR EQU3 /= 1 OR EQU4 /= 1 THEN
          FAILED("ERROR IN THE EQUAL OPERATOR");
     END IF;

     IF NEQ1 /= 0 OR NEQ2 /= 1 OR NEQ3 /= 1 OR NEQ4 /= 0 THEN
          FAILED("ERROR IN THE NOT EQUAL OPERATOR");
     END IF;

     RESULT;

END C49022C;
