-- C45253A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES "A IN T" AND "A NOT IN T" GIVE
-- APPROPRIATE RESULTS, EVEN WHEN USER-DEFINED ORDERING OPERATORS EXIST
-- FOR T.

-- WRG 8/27/86
-- JRL 06/12/96  Added function The_Delta. Eliminated static expressions
--               outside the base range of type T.

WITH REPORT; USE REPORT;
PROCEDURE C45253A IS

     TYPE FIXED IS DELTA 0.25 RANGE 0.0 .. 1000.0;
     TYPE T IS NEW FIXED;

     FUNCTION "<" (LEFT, RIGHT : T) RETURN BOOLEAN IS
     BEGIN
          RETURN FIXED (LEFT) >= FIXED (RIGHT);
     END "<";

     FUNCTION "<=" (LEFT, RIGHT : T) RETURN BOOLEAN IS
     BEGIN
          RETURN FIXED (LEFT) > FIXED (RIGHT);
     END "<=";

     FUNCTION ">" (LEFT, RIGHT : T) RETURN BOOLEAN IS
     BEGIN
          RETURN FIXED (LEFT) <= FIXED (RIGHT);
     END ">";

     FUNCTION ">=" (LEFT, RIGHT : T) RETURN BOOLEAN IS
     BEGIN
          RETURN FIXED (LEFT) < FIXED (RIGHT);
     END ">=";

     function The_Delta return T is
     begin
        return T'Delta;
     end The_Delta;

BEGIN

     TEST ("C45253A", "CHECK THAT FOR FIXED POINT TYPES ""A IN T"" " &
                      "AND ""A NOT IN T"" GIVE APPROPRIATE RESULTS, " &
                      "EVEN WHEN USER-DEFINED ORDERING OPERATORS " &
                      "EXIST FOR T");

     IF IDENT_INT (1) * 0.0 NOT IN T THEN
          FAILED ("0.0 NOT IN T");
     END IF;

-- 06/12/96      IF IDENT_INT (1) * 1000.0 NOT IN T THEN
     if Ident_Int (2) * 500.0 not in T then
          FAILED ("1000.0 NOT IN T");
     END IF;

-- 06/12/96      IF IDENT_INT (1) * (-0.25) IN T THEN
     if Ident_Int (1) * (-The_Delta) in T then
          FAILED ("-0.25 IN T");
     END IF;

-- 06/12/96      IF IDENT_INT (1) * 1000.25 IN T THEN
     if Ident_Int (2) * 500.0 + The_Delta in T then
          FAILED ("1000.25 IN T");
     END IF;

-- 06/12/96      IF IDENT_INT (1) * (-1000.0) IN T THEN
     if Ident_Int (2) * (-500.0) in T then
          FAILED ("-1000.0 IN T");
     END IF;

     RESULT;

END C45253A;
