-- C35A07A.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE FIRST AND LAST ATTRIBUTES YIELD
-- CORRECT VALUES.

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/25/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
PROCEDURE C35A07A IS

     -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
     -- 'MANTISSA VALUE.

     TYPE MIDDLE_M3         IS DELTA 0.5   RANGE  0.0 .. 2.5;
     TYPE MIDDLE_M15        IS DELTA 2.0 **(-6) RANGE  -512.0 ..  512.0;
     TYPE LIKE_DURATION_M23 IS DELTA 0.020 RANGE -86_400.0 .. 86_400.0;
     TYPE DECIMAL_M18       IS DELTA 0.1   RANGE -10_000.0 .. 10_000.0;
     TYPE DECIMAL_M4        IS DELTA 100.0 RANGE   -1000.0 ..   1000.0;
          -- LARGEST MODEL NUMBER IS 960.0.

     -------------------------------------------------------------------

     SUBTYPE ST_LEFT_EDGE_M6 IS MIDDLE_M15
          DELTA 2.0 ** (-6) RANGE IDENT_INT (1) * (-1.0) .. 1.0;
     SUBTYPE ST_MIDDLE_M3    IS LIKE_DURATION_M23
          DELTA 0.5 RANGE  0.0 .. 2.5;
     SUBTYPE ST_DECIMAL_M7   IS DECIMAL_M18
          DELTA  10.0 RANGE -1000.0 .. 1000.0;
          -- LARGEST MODEL NUMBER IS 1016.0.
     SUBTYPE ST_DECIMAL_M3   IS DECIMAL_M4
          DELTA 100.0 RANGE  -500.0 ..  500.0;
          -- LARGEST MODEL NUMBER IS 448.0.
     SUBTYPE ST_MIDDLE_M15   IS MIDDLE_M15
          RANGE 6.0 .. 3.0;

BEGIN

     TEST ("C35A07A", "CHECK THAT FOR FIXED POINT TYPES THE FIRST " &
                      "AND LAST ATTRIBUTES YIELD CORRECT VALUES - " &
                      "BASIC TYPES");

     -------------------------------------------------------------------


     IF MIDDLE_M3'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("MIDDLE_M3'FIRST /= 0.0");
     END IF;
     IF MIDDLE_M3'LAST /= IDENT_INT (1) * 2.5 THEN
          FAILED ("MIDDLE_M3'LAST /= 2.5");
     END IF;

     -------------------------------------------------------------------


     IF LIKE_DURATION_M23'FIRST /= IDENT_INT (1) * (-86_400.0) THEN
          FAILED ("LIKE_DURATION_M23'FIRST /= -86_400.0");
     END IF;
     IF LIKE_DURATION_M23'LAST  /= IDENT_INT (1) * 86_400.0 THEN
          FAILED ("LIKE_DURATION_M23'LAST  /=  86_400.0");
     END IF;

     -------------------------------------------------------------------

     IF DECIMAL_M18'FIRST /= IDENT_INT (1) * (-10_000.0) THEN
          FAILED ("DECIMAL_M18'FIRST /= -10_000.0");
     END IF;
     IF DECIMAL_M18'LAST /= IDENT_INT (1) * 10_000.0 THEN
          FAILED ("DECIMAL_M18'LAST /= 10_000.0");
     END IF;

     -------------------------------------------------------------------


     IF ST_MIDDLE_M3'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("ST_MIDDLE_M3'FIRST /= 0.0");
     END IF;
     IF ST_MIDDLE_M3'LAST /= IDENT_INT (1) * 2.5 THEN
          FAILED ("ST_MIDDLE_M3'LAST /= 2.5");
     END IF;

     -------------------------------------------------------------------

     IF ST_DECIMAL_M7'FIRST /= IDENT_INT (1) * (-1000.0) THEN
          FAILED ("ST_DECIMAL_M7'FIRST /= -1000.0");
     END IF;
     IF ST_DECIMAL_M7'LAST /= IDENT_INT (1) * 1000.0 THEN
          FAILED ("ST_DECIMAL_M7'LAST /= 1000.0");
     END IF;

     -------------------------------------------------------------------


     IF ST_MIDDLE_M15'FIRST /= IDENT_INT (1) * 6.0 THEN
          FAILED ("ST_MIDDLE_M15'FIRST /= 6.0");
     END IF;
     IF ST_MIDDLE_M15'LAST /= IDENT_INT (1) * 3.0 THEN
          FAILED ("ST_MIDDLE_M15'LAST /= 3.0");
     END IF;

     -------------------------------------------------------------------

     RESULT;

END C35A07A;
