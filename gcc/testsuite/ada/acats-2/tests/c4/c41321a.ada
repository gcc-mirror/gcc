-- C41321A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED RELATIONAL OPERATORS, LOGICAL 
-- OPERATORS, AND THE "NOT" OPERATOR MAY BE SELECTED FROM OUTSIDE
-- THE PACKAGE USING AN EXPANDED NAME, FOR A DERIVED BOOLEAN TYPE.

-- TBN  7/16/86

WITH REPORT; USE REPORT;
PROCEDURE C41321A IS

     PACKAGE P IS
          TYPE DERIVED_BOOLEAN IS NEW BOOLEAN RANGE FALSE .. TRUE;
          DERIVED_FALSE : DERIVED_BOOLEAN := FALSE;
          DERIVED_TRUE : DERIVED_BOOLEAN := TRUE;
     END P;

     DBOOL_FALSE : P.DERIVED_BOOLEAN := P.FALSE;
     DBOOL_TRUE : P.DERIVED_BOOLEAN := P.TRUE;

BEGIN
     TEST ("C41321A", "CHECK THAT IMPLICITLY DECLARED RELATIONAL " &
                      "OPERATORS, LOGICAL OPERATORS, AND THE 'NOT' " &
                      "OPERATOR MAY BE SELECTED FROM OUTSIDE THE " &
                      "PACKAGE USING AN EXPANDED NAME, FOR A DERIVED " &
                      "BOOLEAN TYPE");

     IF P."=" (DBOOL_FALSE, P.DERIVED_TRUE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."/=" (DBOOL_TRUE, P.DERIVED_TRUE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     IF P."<" (P.DERIVED_TRUE, P.DERIVED_FALSE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
     END IF;

     IF P.">" (DBOOL_TRUE, P.DERIVED_TRUE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
     END IF;

     IF P."<=" (P.DERIVED_TRUE, DBOOL_FALSE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
     END IF;

     IF P."<=" (P.DERIVED_TRUE, DBOOL_TRUE) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
     END IF;

     IF P.">=" (P.DERIVED_TRUE, DBOOL_TRUE) THEN
          NULL;
     ELSE
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 7");
     END IF;

     FOR J IN P.DERIVED_BOOLEAN'(P.TRUE) .. P.DERIVED_BOOLEAN'(P.TRUE)
          LOOP
               IF P.">=" (DBOOL_FALSE, J) THEN
                    FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 8");
               END IF;
          END LOOP;

     IF P."AND" (DBOOL_FALSE, P.DERIVED_TRUE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 9");
     END IF;

     IF P."OR" (DBOOL_FALSE, P.DERIVED_FALSE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 10");
     END IF;

     IF P."XOR" (DBOOL_TRUE, P.DERIVED_TRUE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 11");
     END IF;

     IF P."NOT" (P.DERIVED_TRUE) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 12");
     END IF;

     RESULT;
END C41321A;
