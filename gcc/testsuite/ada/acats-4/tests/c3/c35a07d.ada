-- C35A07D.ADA

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

-- CASE D: TYPES TYPICAL OF APPLICATIONS USING FIXED POINT ARITHMETIC.

-- WRG 8/25/86
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C35A07D IS

     PI      : CONSTANT := 3.14159_26535_89793_23846;
     TWO_PI  : CONSTANT := 2 * PI;
     HALF_PI : CONSTANT := PI / 2;

     MM : CONSTANT := MAX_MANTISSA;

     -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
     -- 'MANTISSA VALUE.

     TYPE PIXEL_M10 IS DELTA 1.0 / 1024.0 RANGE 0.0 ..  1.0;
     TYPE RULER_M8  IS DELTA 1.0 / 16.0   RANGE 0.0 .. 12.0;

     TYPE HOURS_M16 IS DELTA   24.0 * 2.0 ** (-15) RANGE 0.0 ..   24.0;
     TYPE MILES_M16 IS DELTA 3000.0 * 2.0 ** (-15) RANGE 0.0 .. 3000.0;

     TYPE SYMMETRIC_DEGREES_M7  IS
          DELTA 2.0         RANGE -180.0 .. 180.0;
     TYPE NATURAL_DEGREES_M15   IS
          DELTA 2.0 ** (-6) RANGE    0.0 .. 360.0;
     TYPE SYMMETRIC_RADIANS_M16 IS
          DELTA     PI * 2.0 ** (-15) RANGE -PI .. PI;
               -- 'SMALL = 2.0 ** (-14) = 0.00006_10351_5625.
     TYPE NATURAL_RADIANS_M8    IS
          DELTA TWO_PI * 2.0 ** ( -7) RANGE 0.0 .. TWO_PI;
               -- 'SMALL = 2.0 ** ( -5) = 0.03125.

     -------------------------------------------------------------------

     SUBTYPE ST_MILES_M8             IS MILES_M16
          DELTA 3000.0 * 2.0 ** (-15) RANGE 0.0 .. 10.0;
     SUBTYPE ST_NATURAL_DEGREES_M11  IS NATURAL_DEGREES_M15
          DELTA 0.25 RANGE 0.0 .. 360.0;
     SUBTYPE ST_SYMMETRIC_RADIANS_M8 IS SYMMETRIC_RADIANS_M16
          DELTA HALF_PI * 2.0 ** (-7) RANGE -HALF_PI .. HALF_PI;
               -- 'SMALL = 2.0 ** ( -7) = 0.00781_25.

BEGIN

     TEST ("C35A07D", "CHECK THAT FOR FIXED POINT TYPES THE FIRST " &
                      "AND LAST ATTRIBUTES YIELD CORRECT VALUES - " &
                      "TYPICAL TYPES");

     -------------------------------------------------------------------


     IF PIXEL_M10'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("PIXEL_M10'FIRST /= 0.0");
     END IF;

     -------------------------------------------------------------------

     IF RULER_M8'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("RULER_M8'FIRST /= 0.0");
     END IF;
     IF RULER_M8'LAST /= IDENT_INT (1) * 12.0 THEN
          FAILED ("RULER_M8'LAST /= 12.0");
     END IF;

     -------------------------------------------------------------------

     IF HOURS_M16'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("HOURS_M16'FIRST /= 0.0");
     END IF;
     IF HOURS_M16'LAST /= IDENT_INT (1) * 24.0 THEN
          FAILED ("HOURS_M16'LAST /= 24.0");
     END IF;

     -------------------------------------------------------------------

     IF MILES_M16'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("MILES_M16'FIRST /= 0.0");
     END IF;
     IF MILES_M16'LAST /= IDENT_INT (1) * 3000.0 THEN
          FAILED ("MILES_M16'LAST /= 3000.0");
     END IF;

     -------------------------------------------------------------------

     IF SYMMETRIC_DEGREES_M7'FIRST /= IDENT_INT (1) * (-180.0) THEN
          FAILED ("SYMMETRIC_DEGREES_M7'FIRST /= -180.0");
     END IF;
     IF SYMMETRIC_DEGREES_M7'LAST /= IDENT_INT (1) * 180.0 THEN
          FAILED ("SYMMETRIC_DEGREES_M7'LAST /= 180.0");
     END IF;

     -------------------------------------------------------------------

     IF NATURAL_DEGREES_M15'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("NATURAL_DEGREES_M15'FIRST /= 0.0");
     END IF;
     IF NATURAL_DEGREES_M15'LAST /= IDENT_INT (1) * 360.0 THEN
          FAILED ("NATURAL_DEGREES_M15'LAST /= 360.0");
     END IF;

     -------------------------------------------------------------------

     -- PI IS IN 3.0 + 2319 * 'SMALL .. 3.0 + 2320 * 'SMALL.
     IF SYMMETRIC_RADIANS_M16'FIRST NOT IN
        -3.14160_15625 .. -3.14154_05273_4375 THEN
          FAILED ("SYMMETRIC_RADIANS_M16'FIRST NOT IN " &
                  "-3.14160_15625 .. -3.14154_05273_4375");
     END IF;
     IF SYMMETRIC_RADIANS_M16'LAST  NOT IN
        3.14154_05273_4375 .. 3.14160_15625 THEN
          FAILED ("SYMMETRIC_RADIANS_M16'LAST NOT IN " &
                  "3.14154_05273_4375 .. 3.14160_15625");
     END IF;

     -------------------------------------------------------------------

     IF NATURAL_RADIANS_M8'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("NATURAL_RADIANS_M8'FIRST /= 0.0");
     END IF;
     -- TWO_PI IS IN 201 * 'SMALL .. 202 * 'SMALL.
     IF NATURAL_RADIANS_M8'LAST  NOT IN 6.28125 .. 6.3125 THEN
          FAILED ("NATURAL_RADIANS_M8'LAST NOT IN 6.28125 .. 6.3125");
     END IF;

     -------------------------------------------------------------------

     IF ST_MILES_M8'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("ST_MILES_M8'FIRST /= 0.0");
     END IF;
     IF ST_MILES_M8'LAST /= IDENT_INT (1) * 10.0 THEN
          FAILED ("ST_MILES_M8'LAST /= 10.0");
     END IF;

     -------------------------------------------------------------------

     IF ST_NATURAL_DEGREES_M11'FIRST /= IDENT_INT (1) * 0.0 THEN
          FAILED ("ST_NATURAL_DEGREES_M11'FIRST /= 0.0");
     END IF;
     IF ST_NATURAL_DEGREES_M11'LAST /= IDENT_INT (1) * 360.0 THEN
          FAILED ("ST_NATURAL_DEGREES_M11'LAST /= 360.0");
     END IF;

     -------------------------------------------------------------------

     -- HALF_PI IS IN 201 * 'SMALL .. 202 * 'SMALL.
     IF ST_SYMMETRIC_RADIANS_M8'FIRST NOT IN
        -1.57812_5 .. -1.57031_25 THEN
          FAILED ("ST_SYMMETRIC_RADIANS_M8'FIRST NOT IN " &
                  "-1.57812_5 .. -1.57031_25");
     END IF;
     IF ST_SYMMETRIC_RADIANS_M8'LAST  NOT IN
        1.57031_25 .. 1.57812_5 THEN
          FAILED ("ST_SYMMETRIC_RADIANS_M8'LAST NOT IN " &
                  "1.57031_25 .. 1.57812_5");
     END IF;

     -------------------------------------------------------------------

     RESULT;

END C35A07D;
