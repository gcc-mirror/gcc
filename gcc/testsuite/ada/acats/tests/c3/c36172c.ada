-- C36172C.ADA

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
-- CHECK THAT NO EXCEPTION IS RAISED FOR A NULL ARRAY WHOSE DIFFERENCE
-- IN BOUNDS LIES OUTSIDE THE INDEX BASE TYPE.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X

-- JBG 6/5/85
-- JRL 03/30/93 REMOVED NUMERIC_ERROR FROM TEST.

WITH REPORT; USE REPORT;
PROCEDURE C36172C IS
BEGIN
     TEST ("C36172C", "CHECK THAT NO EXCEPTION IS RAISED FOR A NULL " &
                      "ARRAY WHOSE DIFFERENCE IN BOUNDS LIES OUTSIDE " &
                      "THE INDEX BASE TYPE");

     BEGIN
          DECLARE
               V : STRING (INTEGER'LAST .. -2);
          BEGIN
               IF NOT EQUAL (V'FIRST, V'FIRST) THEN
                    FAILED ("IMPOSSIBLE");
               END IF;
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED");
     END;

     RESULT;
END C36172C;
