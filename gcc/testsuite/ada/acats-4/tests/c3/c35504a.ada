-- C35504A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED WHEN THE USER-DEFINED 
-- ENUMERATION ARGUMENT TO 'SUCC, 'PRED, 'POS, 'VAL, 'IMAGE, AND 'VALUE
-- IS NOT IN THE ATTRIBUTED SUBTYPE'S RANGE CONSTRAINT.

-- DAT 3/18/81
-- SPS 01/13/83

WITH REPORT; USE REPORT;

PROCEDURE C35504A IS

     TYPE E IS (A, 'A', B, 'B', C, 'C', D, 'D', XYZ);

     SUBTYPE S IS E RANGE B .. C;

BEGIN
     TEST ("C35504A", "CONSTRAINT_ERROR IS NOT RAISED IN T'SUCC(X),"
          & " T'PRED(X), T'POS(X), T'VAL(X), T'IMAGE(X), AND"
          & " T'VALUE(X) WHEN THE VALUES ARE NOT WITHIN T'S"
          & " RANGE CONSTRAINT, FOR USER-DEFINED ENUMERATION TYPES");

     BEGIN
          FOR X IN E LOOP
               IF (X /= A AND THEN S'SUCC(S'PRED(X)) /= X)
               OR (X /= XYZ AND THEN S'PRED(S'SUCC(X)) /= X)
               OR S'VAL(S'POS(X)) /= X
               OR S'VALUE(S'IMAGE(X)) /= X
               THEN
                    FAILED ("WRONG ATTRIBUTE VALUE");
               END IF;
          END LOOP;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => FAILED ("CONSTRAINT_ERROR RAISED"
                    & " WHEN IT SHOULDN'T HAVE BEEN");
          WHEN OTHERS => FAILED ("INCORRECT EXCEPTION RAISED");
     END;

     RESULT;
END C35504A;
