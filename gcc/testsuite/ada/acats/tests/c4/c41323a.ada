-- C41323A.ADA

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
-- OPERATORS (+, -, *, /, **, ABS) MAY BE SELECTED FROM OUTSIDE THE
-- PACKAGE USING AN EXPANDED NAME, FOR A FLOATING POINT TYPE.

-- TBN  7/16/86

WITH REPORT; USE REPORT;
PROCEDURE C41323A IS

     PACKAGE P IS
          TYPE FLOAT IS DIGITS 5 RANGE -1.0E1 .. 1.0E1;
          OBJ_FLO_1 : FLOAT := -5.5;
          OBJ_FLO_2 : FLOAT := 1.5;
          OBJ_FLO_3 : FLOAT := 10.0;
     END P;

     FLO_VAR : P.FLOAT;
     FLO_VAR_1 : P.FLOAT := P."-"(P.FLOAT'(5.5));
     FLO_VAR_2 : P.FLOAT := P.FLOAT'(1.5);
     FLO_VAR_3 : P.FLOAT := P.FLOAT'(1.0E1);

BEGIN
     TEST ("C41323A", "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
                      "OPERATORS AND ARITHMETIC OPERATORS (+, -, *, " &
                      "/, **, ABS) MAY BE SELECTED FROM OUTSIDE THE " &
                      "PACKAGE USING AN EXPANDED NAME, FOR A " &
                      "FLOATING POINT TYPE");

     IF P."=" (FLO_VAR_1, P."-"(P.FLOAT'(5.55))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."/=" (FLO_VAR_1, P.OBJ_FLO_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     IF P."<" (FLO_VAR_2, P.OBJ_FLO_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
     END IF;

     IF P.">" (FLO_VAR_2, P.OBJ_FLO_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
     END IF;

     IF P."<=" (FLO_VAR_3, P.FLOAT'(9.9)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
     END IF;

     IF P."<=" (FLO_VAR_3, P.FLOAT'(10.0)) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
     END IF;

     IF P.">=" (P.OBJ_FLO_2, FLO_VAR_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
     END IF;

     IF P.">=" (P.OBJ_FLO_3, FLO_VAR_3) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
     END IF;

     FLO_VAR := P."+" (FLO_VAR_1, P.OBJ_FLO_2);
     IF P."/=" (FLO_VAR, P."-"(P.FLOAT'(4.0))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
     END IF;

     FLO_VAR := P."+" (FLO_VAR_1);
     IF P."/=" (FLO_VAR, P.OBJ_FLO_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
     END IF;

     FLO_VAR := P."-" (FLO_VAR_2, P.OBJ_FLO_1);
     IF P."/=" (FLO_VAR, P.FLOAT'(7.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
     END IF;

     FLO_VAR := P."*" (FLO_VAR_2, P.FLOAT'(2.0));
     IF P."/=" (FLO_VAR, P.FLOAT'(3.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
     END IF;

     FLO_VAR := P."/" (FLO_VAR_3, P.FLOAT'(2.0));
     IF P."/=" (FLO_VAR, P.FLOAT'(5.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
     END IF;

     FLO_VAR := P."**" (P.FLOAT'(2.0), 3);
     IF P."/=" (FLO_VAR, P.FLOAT'(8.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
     END IF;

     FLO_VAR := P."ABS" (FLO_VAR_1);
     IF P."/=" (FLO_VAR, P.FLOAT'(5.5)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 15");
     END IF;

     RESULT;
END C41323A;
