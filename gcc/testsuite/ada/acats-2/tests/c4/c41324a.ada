-- C41324A.ADA

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
-- OPERATORS (+, -, *, /, ABS) MAY BE SELECTED FROM OUTSIDE THE
-- PACKAGE USING AN EXPANDED NAME, FOR A FIXED POINT TYPE.

-- TBN  7/16/86

WITH REPORT; USE REPORT;
PROCEDURE C41324A IS

     PACKAGE P IS
          TYPE FIXED IS DELTA 0.125 RANGE -1.0E1 .. 1.0E1;
          OBJ_FIX_1 : FIXED := -5.5;
          OBJ_FIX_2 : FIXED := 1.5;
          OBJ_FIX_3 : FIXED := 10.0;
     END P;

     FIX_VAR : P.FIXED;
     FIX_VAR_1 : P.FIXED := P."-"(P.FIXED'(5.5));
     FIX_VAR_2 : P.FIXED := P.FIXED'(1.5);
     FIX_VAR_3 : P.FIXED := P.FIXED'(1.0E1);

BEGIN
     TEST ("C41324A", "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
                      "OPERATORS AND ARITHMETIC OPERATORS (+, -, *, " &
                      "/, ABS) MAY BE SELECTED FROM OUTSIDE THE " &
                      "PACKAGE USING AN EXPANDED NAME, FOR A FIXED " &
                      "POINT TYPE");

     IF P."=" (FIX_VAR_1, P."-"(P.FIXED'(6.0))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."/=" (FIX_VAR_1, P.OBJ_FIX_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     IF P."<" (FIX_VAR_2, P.OBJ_FIX_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
     END IF;

     IF P.">" (FIX_VAR_2, P.OBJ_FIX_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
     END IF;

     IF P."<=" (FIX_VAR_3, P.FIXED'(9.9)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
     END IF;

     IF P."<=" (FIX_VAR_3, P.FIXED'(10.0)) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
     END IF;

     IF P.">=" (P.OBJ_FIX_2, FIX_VAR_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
     END IF;

     IF P.">=" (P.OBJ_FIX_2, FIX_VAR_2) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
     END IF;

     FIX_VAR := P."+" (FIX_VAR_1, P.OBJ_FIX_2);
     IF P."/=" (FIX_VAR, P."-"(P.FIXED'(4.0))) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
     END IF;

     FIX_VAR := P."-" (FIX_VAR_2, P.OBJ_FIX_1);
     IF P."/=" (FIX_VAR, P.FIXED'(7.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
     END IF;

     FIX_VAR := P."*" (FIX_VAR_2, 2);
     IF P."/=" (FIX_VAR, P.FIXED'(3.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
     END IF;

     FIX_VAR := P."*" (3, FIX_VAR_2);
     IF P."/=" (FIX_VAR, P.FIXED'(4.5)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
     END IF;

     FIX_VAR := P."/" (FIX_VAR_3, 2);
     IF P."/=" (FIX_VAR, P.FIXED'(5.0)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 13");
     END IF;

     FIX_VAR := P."ABS" (FIX_VAR_1);
     IF P."/=" (FIX_VAR, P.FIXED'(5.5)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 14");
     END IF;

     RESULT;
END C41324A;
