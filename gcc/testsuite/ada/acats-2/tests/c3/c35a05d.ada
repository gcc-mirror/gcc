-- C35A05D.ADA

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
-- CHECK THAT FOR FIXED POINT TYPES THE FORE AND AFT ATTRIBUTES YIELD
-- THE CORRECT VALUES.

-- CASE D: TYPES TYPICAL OF APPLICATIONS USING FIXED POINT ARITHMETIC.

-- WRG 8/14/86

WITH REPORT; USE REPORT;
PROCEDURE C35A05D IS

     PI      : CONSTANT := 3.14159_26535_89793_23846;
     TWO_PI  : CONSTANT := 2 * PI;
     HALF_PI : CONSTANT := PI / 2;

     MM : CONSTANT := 23;

     -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
     -- 'MANTISSA VALUE.

     TYPE MICRO_ANGLE_ERROR_M15  IS
          DELTA 16.0  RANGE -(2.0 ** 19) .. 2.0 ** 19;
     TYPE TRACK_RANGE_M15        IS
          DELTA 0.125 RANGE -(2.0 ** 12) .. 2.0 ** 12;
     TYPE SECONDS_MM             IS
          DELTA 2.0 ** (8 - MM) RANGE -(2.0 ** 8) .. 2.0 ** 8;
     TYPE RANGE_CELL_MM          IS
          DELTA 2.0 ** (-5)
          RANGE -(2.0 ** (MM - 5) ) .. 2.0 ** (MM - 5);

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
     TYPE NATURAL_RADIANS_M8    IS
          DELTA TWO_PI * 2.0 ** ( -7) RANGE 0.0 .. TWO_PI;

     -------------------------------------------------------------------

     SUBTYPE ST_MILES_M8             IS MILES_M16
          DELTA 3000.0 * 2.0 ** (-15) RANGE 0.0 .. 10.0;
     SUBTYPE ST_NATURAL_DEGREES_M11  IS NATURAL_DEGREES_M15
          DELTA 0.25 RANGE 0.0 .. 360.0;
     SUBTYPE ST_SYMMETRIC_RADIANS_M8 IS SYMMETRIC_RADIANS_M16
          DELTA HALF_PI * 2.0 ** (-7) RANGE -HALF_PI .. HALF_PI;

     -------------------------------------------------------------------

     PROCEDURE CHECK_FORE_AND_AFT
         (NAME        : STRING;
          ACTUAL_FORE : INTEGER; CORRECT_FORE : POSITIVE;
          ACTUAL_AFT  : INTEGER; CORRECT_AFT  : POSITIVE) IS
     BEGIN
          IF ACTUAL_FORE /= IDENT_INT (CORRECT_FORE) THEN
               FAILED (NAME & "'FORE =" & INTEGER'IMAGE(ACTUAL_FORE) );
          END IF;
          IF ACTUAL_AFT /= IDENT_INT (CORRECT_AFT) THEN
               FAILED (NAME & "'AFT  =" & INTEGER'IMAGE(ACTUAL_AFT) );
          END IF;
     END CHECK_FORE_AND_AFT;

BEGIN

     TEST ("C35A05D", "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
                      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " &
                      "TYPICAL TYPES");

     CHECK_FORE_AND_AFT ("MICRO_ANGLE_ERROR_M15",
                                         MICRO_ANGLE_ERROR_M15'FORE, 7,
                                         MICRO_ANGLE_ERROR_M15'AFT,  1);

     CHECK_FORE_AND_AFT ("TRACK_RANGE_M15", TRACK_RANGE_M15'FORE, 5,
                                            TRACK_RANGE_M15'AFT,  1);

     CHECK_FORE_AND_AFT ("SECONDS_MM",      SECONDS_MM'FORE, 4,
                                            SECONDS_MM'AFT,  5);

     CHECK_FORE_AND_AFT ("RANGE_CELL_MM",   RANGE_CELL_MM'FORE, 7,
                                            RANGE_CELL_MM'AFT,  2);

     CHECK_FORE_AND_AFT ("PIXEL_M10",       PIXEL_M10'FORE, 2,
                                            PIXEL_M10'AFT,  4);

     CHECK_FORE_AND_AFT ("RULER_M8",        RULER_M8'FORE, 3,
                                            RULER_M8'AFT,  2);

     CHECK_FORE_AND_AFT ("HOURS_M16",       HOURS_M16'FORE, 3,
                                            HOURS_M16'AFT,  4);

     CHECK_FORE_AND_AFT ("MILES_M16",       MILES_M16'FORE, 5,
                                            MILES_M16'AFT,  2);

     CHECK_FORE_AND_AFT ("SYMMETRIC_DEGREES_M7",
                                         SYMMETRIC_DEGREES_M7'FORE, 4,
                                         SYMMETRIC_DEGREES_M7'AFT,  1);

     CHECK_FORE_AND_AFT ("NATURAL_DEGREES_M15",
                                         NATURAL_DEGREES_M15'FORE, 4,
                                         NATURAL_DEGREES_M15'AFT,  2);

     CHECK_FORE_AND_AFT ("SYMMETRIC_RADIANS_M16",
                                         SYMMETRIC_RADIANS_M16'FORE, 2,
                                         SYMMETRIC_RADIANS_M16'AFT,  5);

     CHECK_FORE_AND_AFT ("NATURAL_RADIANS_M8",
                                         NATURAL_RADIANS_M8'FORE, 2,
                                         NATURAL_RADIANS_M8'AFT,  2);

     CHECK_FORE_AND_AFT ("ST_MILES_M8",  ST_MILES_M8'FORE, 3,
                                         ST_MILES_M8'AFT,  2);

     CHECK_FORE_AND_AFT ("ST_NATURAL_DEGREES_M11",
                                       ST_NATURAL_DEGREES_M11'FORE, 4,
                                       ST_NATURAL_DEGREES_M11'AFT,  1);

     CHECK_FORE_AND_AFT ("ST_SYMMETRIC_RADIANS_M8",
                                       ST_SYMMETRIC_RADIANS_M8'FORE, 2,
                                       ST_SYMMETRIC_RADIANS_M8'AFT,  2);

     RESULT;

END C35A05D;
