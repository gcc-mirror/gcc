-- C41322A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED RELATIONAL OPERATORS AND ARITHMETIC
-- OPERATORS (+, -, *, /, **, ABS, MOD, REM) MAY BE SELECTED FROM
-- OUTSIDE THE PACKAGE USING AN EXPANDED NAME, FOR AN INTEGER TYPE.

-- TBN  7/16/86

WITH REPORT; USE REPORT;
PROCEDURE C41322A IS

     PACKAGE P IS
          TYPE INT IS RANGE -10 .. 10;
          OBJ_INT_1 : INT := -10;
          OBJ_INT_2 : INT := 1;
          OBJ_INT_3 : INT := 10;
     END P;

     INT_VAR : P.INT;
     INT_VAR_1 : P.INT := P."-"(P.INT'(10));
     INT_VAR_2 : P.INT := P.INT'(1);
     INT_VAR_3 : P.INT := P.INT'(10);

BEGIN
     TEST ("C41322A", "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
                      "OPERATORS AND ARITHMETIC OPERATORS (+, -, *, " &
                      "/, **, ABS, MOD, REM) MAY BE SELECTED FROM " &
                      "OUTSIDE THE PACKAGE USING AN EXPANDED NAME, " &
                      "FOR AN INTEGER TYPE");

     IF P."=" (INT_VAR_1, P.INT'(2)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."/=" (INT_VAR_1, P.OBJ_INT_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     IF P."<" (INT_VAR_2, 0) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
     END IF;

     IF P.">" (INT_VAR_2, P.OBJ_INT_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
     END IF;

     IF P."<=" (INT_VAR_3, P.INT'(9)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
     END IF;

     FOR J IN P.INT'(4) .. P.INT'(4) LOOP
          IF P.">=" (J, INT_VAR_3) THEN
               FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
          END IF;
     END LOOP;

     INT_VAR := P."+" (INT_VAR_1, P.INT'(2));
     IF P."/=" (INT_VAR, P."-"(P.INT'(8))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
     END IF;

     INT_VAR := P."+" (P.INT'(2));
     IF P."/=" (INT_VAR, P.INT'(2)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
     END IF;

     INT_VAR := P."-" (INT_VAR_2, P.INT'(0));
     IF P."/=" (INT_VAR, P.OBJ_INT_2) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
     END IF;

     INT_VAR := P."*" (INT_VAR_2, P.INT'(5));
     IF P."/=" (INT_VAR, P.INT'(5)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
     END IF;

     INT_VAR := P."/" (INT_VAR_3, P.INT'(2));
     IF P."/=" (INT_VAR, P.INT'(5)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
     END IF;

     INT_VAR := P."**" (P.INT'(2), 3);
     IF P."/=" (INT_VAR, P.INT'(8)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
     END IF;

     INT_VAR := P."ABS" (INT_VAR_1);
     IF P."/=" (INT_VAR, P.OBJ_INT_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
     END IF;

     INT_VAR := P."MOD" (INT_VAR_1, P.INT'(3));
     IF P."/=" (INT_VAR, P.INT'(2)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
     END IF;

     INT_VAR := P."REM" (INT_VAR_1, P.INT'(3));
     IF P."/=" (INT_VAR, P."-" (INT_VAR_2)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 15");
     END IF;

     RESULT;
END C41322A;
