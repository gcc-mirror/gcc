-- C87B50A.ADA

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
-- CHECK THAT A FUNCTION RENAMING DECLARATION CAN RESOLVE AND RENAME AN
-- OVERLOADED ENUMERATION LITERAL.

-- GOM 11/29/84
-- JWC 7/12/85
-- PWB 03/06/86  CORRECTED ERROR: ADDED "USE" CLAUSE TO MAKE
--               "/=" VISIBLE.

WITH REPORT; USE REPORT;
PROCEDURE C87B50A IS

BEGIN
     TEST ("C87B50A", "CHECK THAT A FUNCTION RENAMING DECLARATION " &
                      "CAN RESOLVE AND RENAME AN OVERLOADED " &
                      "ENUMERATION LITERAL");

     DECLARE

          PACKAGE A IS
               TYPE COLORS IS (RED,GREEN);
               TYPE LIGHT  IS (BLUE,RED);
          END A;

          PACKAGE B IS
               FUNCTION RED RETURN A.COLORS RENAMES A.RED;
               FUNCTION GREEN RETURN A.COLORS RENAMES A.GREEN;
          END B;

          USE A;      -- TO MAKE /= VISIBLE.

     BEGIN

          IF (A.RED /= B.RED) OR (A.GREEN /= B.GREEN) THEN
               FAILED ("RENAMED VALUES NOT EQUAL");
          END IF;

     END;

     RESULT;
END C87B50A;
