-- C49022A.ADA

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
-- CHECK THAT NAMED NUMBER DECLARATIONS (INTEGER) MAY USE EXPRESSIONS
-- WITH INTEGERS.

-- BAW 29 SEPT 80
-- TBN 10/28/85     RENAMED FROM C4A001A.ADA. ADDED RELATIONAL
--                  OPERATORS AND USE OF NAMED NUMBERS.

WITH REPORT;
PROCEDURE C49022A IS

     USE REPORT;

     ADD1 : CONSTANT :=   1   +  1;
     ADD2 : CONSTANT :=   1   + (-1);
     ADD3 : CONSTANT := (-1)  +   1;
     ADD4 : CONSTANT := (-1)  + (-1);
     SUB1 : CONSTANT :=   1   -   1;
     SUB2 : CONSTANT :=   1   - (-1);
     SUB3 : CONSTANT := (-1)  -   1;
     SUB4 : CONSTANT := (-1)  - (-1);
     MUL1 : CONSTANT :=   1   *   1;
     MUL2 : CONSTANT :=   1   * (-1);
     MUL3 : CONSTANT := (-1)  *   1;
     MUL4 : CONSTANT := (-1)  * (-1);
     DIV1 : CONSTANT :=   1   /   1;
     DIV2 : CONSTANT :=   1   / (-1);
     DIV3 : CONSTANT := (-1)  /   1;
     DIV4 : CONSTANT := (-1)  / (-1);
     REM1 : CONSTANT :=  14  REM  5;
     REM2 : CONSTANT :=  14  REM(-5);
     REM3 : CONSTANT :=(-14) REM  5;
     REM4 : CONSTANT :=(-14) REM(-5);
     MOD1 : CONSTANT :=   4  MOD   3;
     MOD2 : CONSTANT :=   4  MOD (-3);
     MOD3 : CONSTANT := (-4) MOD   3;
     MOD4 : CONSTANT := (-4) MOD (-3);
     EXP1 : CONSTANT :=   1  ** 1;
     EXP2 : CONSTANT := (-1) ** 1;
     ABS1 : CONSTANT := ABS( - 10 );
     ABS2 : CONSTANT := ABS( + 10 );
     TOT1 : CONSTANT := ADD1 + SUB1 - MUL1 + DIV1 - REM3 + MOD2 - EXP1;
     LES1 : CONSTANT := BOOLEAN'POS (1 < 2);
     LES2 : CONSTANT := BOOLEAN'POS (1 < (-2));
     LES3 : CONSTANT := BOOLEAN'POS ((-1) < (-2));
     LES4 : CONSTANT := BOOLEAN'POS (ADD1 < SUB1);
     GRE1 : CONSTANT := BOOLEAN'POS (2 > 1);
     GRE2 : CONSTANT := BOOLEAN'POS ((-1) > 2);
     GRE3 : CONSTANT := BOOLEAN'POS ((-1) > (-2));
     GRE4 : CONSTANT := BOOLEAN'POS (ADD1 > SUB1);
     LEQ1 : CONSTANT := BOOLEAN'POS (1 <= 1);
     LEQ2 : CONSTANT := BOOLEAN'POS ((-1) <= 1);
     LEQ3 : CONSTANT := BOOLEAN'POS ((-1) <= (-2));
     LEQ4 : CONSTANT := BOOLEAN'POS (ADD2 <= SUB3);
     GEQ1 : CONSTANT := BOOLEAN'POS (2 >= 1);
     GEQ2 : CONSTANT := BOOLEAN'POS ((-2) >= 1);
     GEQ3 : CONSTANT := BOOLEAN'POS ((-2) >= (-1));
     GEQ4 : CONSTANT := BOOLEAN'POS (ADD2 >= SUB3);
     EQU1 : CONSTANT := BOOLEAN'POS (2 = 2);
     EQU2 : CONSTANT := BOOLEAN'POS ((-2) = 2);
     EQU3 : CONSTANT := BOOLEAN'POS ((-2) = (-2));
     EQU4 : CONSTANT := BOOLEAN'POS (ADD2 = SUB3);
     NEQ1 : CONSTANT := BOOLEAN'POS (2 /= 2);
     NEQ2 : CONSTANT := BOOLEAN'POS ((-2) /= 1);
     NEQ3 : CONSTANT := BOOLEAN'POS ((-2) /= (-2));
     NEQ4 : CONSTANT := BOOLEAN'POS (ADD2 /= SUB3);


BEGIN
     TEST("C49022A","CHECK THAT NAMED NUMBER DECLARATIONS (INTEGER) " &
                    "MAY USE EXPRESSIONS WITH INTEGERS");

     IF ADD1 /= 2 OR ADD2 /= 0 OR ADD3 /= 0 OR ADD4 /= -2 THEN
          FAILED("ERROR IN THE ADDING OPERATOR +");
     END IF;

     IF SUB1 /= 0 OR SUB2 /= 2 OR SUB3 /= -2 OR SUB4 /= 0 THEN
          FAILED("ERROR IN THE ADDING OPERATOR -");
     END IF;

     IF MUL1 /= 1 OR MUL2 /= -1 OR MUL3 /= -1 OR MUL4 /= 1 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR *");
     END IF;

     IF DIV1 /= 1 OR DIV2 /= -1 OR DIV3 /= -1 OR DIV4 /= 1 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR /");
     END IF;

     IF REM1 /= 4 OR REM2 /=  4 OR REM3 /= -4 OR REM4 /= -4 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR REM");
     END IF;

     IF MOD1 /= 1 OR MOD2 /= -2 OR MOD3 /= 2 OR MOD4 /= -1 THEN
          FAILED("ERROR IN THE MULTIPLYING OPERATOR MOD");
     END IF;

     IF EXP1 /= 1 OR EXP2 /= -1 THEN
          FAILED("ERROR IN THE EXPONENTIATING OPERATOR");
     END IF;

     IF ABS1 /= 10 OR ABS2 /= 10 THEN
          FAILED("ERROR IN THE ABS OPERATOR");
     END IF;

     IF TOT1 /= 3 THEN
          FAILED("ERROR IN USING NAMED NUMBERS WITH OPERATORS");
     END IF;

     IF LES1 /= 1 OR LES2 /= 0 OR LES3 /= 0 OR LES4 /= 0 THEN
          FAILED("ERROR IN THE LESS THAN OPERATOR");
     END IF;

     IF GRE1 /= 1 OR GRE2 /= 0 OR GRE3 /= 1 OR GRE4 /= 1 THEN
          FAILED("ERROR IN THE GREATER THAN OPERATOR");
     END IF;

     IF LEQ1 /= 1 OR LEQ2 /= 1 OR LEQ3 /= 0 OR LEQ4 /= 0 THEN
          FAILED("ERROR IN THE LESS THAN EQUAL OPERATOR");
     END IF;

     IF GEQ1 /= 1 OR GEQ2 /= 0 OR GEQ3 /= 0 OR GEQ4 /= 1 THEN
          FAILED("ERROR IN THE GREATER THAN EQUAL OPERATOR");
     END IF;

     IF EQU1 /= 1 OR EQU2 /= 0 OR EQU3 /= 1 OR EQU4 /= 0 THEN
          FAILED("ERROR IN THE EQUAL OPERATOR");
     END IF;

     IF NEQ1 /= 0 OR NEQ2 /= 1 OR NEQ3 /= 0 OR NEQ4 /= 1 THEN
          FAILED("ERROR IN THE NOT EQUAL OPERATOR");
     END IF;

     RESULT;

END C49022A;
