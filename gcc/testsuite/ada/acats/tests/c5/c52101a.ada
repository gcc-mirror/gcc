-- C52101A.ADA

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
-- CHECK THAT ARRAY SUBTYPE CONVERSION IS APPLIED AFTER AN ARRAY VALUE
-- IS DETERMINED.

-- BHS 6/22/84

WITH REPORT;
PROCEDURE C52101A IS

     USE REPORT;

     TYPE DAY IS (MON, TUE, WED, THU, FRI, SAT, SUN);
     SUBTYPE WEEKDAY IS DAY RANGE MON..FRI;

     TYPE ARR IS ARRAY  (WEEKDAY RANGE <>) OF INTEGER;
     TYPE ARR_DAY IS ARRAY (DAY RANGE <>) OF INTEGER;

     NORM     : ARR (MON..FRI);          -- INDEX SUBTYPE WEEKDAY
     NORM_DAY : ARR_DAY (MON..FRI);      -- INDEX SUBTYPE DAY

BEGIN
     TEST ("C52101A", "CHECK THAT ARRAY SUBTYPE CONVERSION " &
                      "APPLIED AFTER ARRAY VAL. DETERMINED");

     BEGIN   -- ILLEGAL CASE
          NORM := (WED..SUN => 0);        -- ERROR: INDEX SUBTYPE

          FAILED ("EXCEPTION NOT RAISED FOR INDEX SUBTYPE ERROR");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ("IMPROPER AGGREGATE BOUNDS DETECTED");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED");

     END;


     BEGIN    -- LEGAL CASE
          NORM_DAY := (WED..FRI => 0, SAT..SUN => 1);
          IF NORM_DAY /= ( 0, 0, IDENT_INT(0), IDENT_INT(1),
                                               IDENT_INT(1)) THEN
               FAILED ("INCORRECT ASSIGNMENT IN LEGAL CASE");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED ON LEGAL INDEX " &
                       "SUBTYPE CONVERSION");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED IN LEGAL CASE");

     END;


     RESULT;

END C52101A;
