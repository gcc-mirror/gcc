-- C35A05Q.ADA

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

-- CASE Q: TYPES TYPICAL OF APPLICATIONS USING FIXED POINT ARITHMETIC,
--         FOR GENERICS.

-- WRG 8/20/86

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C35A05Q IS

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

     TYPE FORE_AND_AFT IS
          RECORD
               FORE, AFT : INTEGER;
          END RECORD;

     GENERIC
          TYPE T IS DELTA <>;
     FUNCTION ATTRIBUTES RETURN FORE_AND_AFT;

     FUNCTION ATTRIBUTES RETURN FORE_AND_AFT IS
     BEGIN
          RETURN ( IDENT_INT (T'FORE), IDENT_INT (T'AFT) );
     END ATTRIBUTES;

     -------------------------------------------------------------------

     PROCEDURE CHECK_ATTRIBUTES
         (NAME : STRING;
          ACTUAL_ATTRIBUTES, CORRECT_ATTRIBUTES : FORE_AND_AFT) IS
     BEGIN
          IF ACTUAL_ATTRIBUTES.FORE /= CORRECT_ATTRIBUTES.FORE THEN
               FAILED ("GENERIC 'FORE FOR " & NAME & " =" &
                       INTEGER'IMAGE(ACTUAL_ATTRIBUTES.FORE) );
          END IF;
          IF ACTUAL_ATTRIBUTES.AFT  /= CORRECT_ATTRIBUTES.AFT  THEN
               FAILED ("GENERIC 'AFT  FOR " & NAME & " =" &
                       INTEGER'IMAGE(ACTUAL_ATTRIBUTES.AFT ) );
          END IF;
     END CHECK_ATTRIBUTES;

     -------------------------------------------------------------------

     FUNCTION FA_MICRO_ANGLE_ERROR_M15
                             IS NEW ATTRIBUTES(MICRO_ANGLE_ERROR_M15  );
     FUNCTION FA_TRACK_RANGE_M15
                             IS NEW ATTRIBUTES(TRACK_RANGE_M15        );
     FUNCTION FA_SECONDS_MM  IS NEW ATTRIBUTES(SECONDS_MM             );
     FUNCTION FA_RANGE_CELL_MM
                             IS NEW ATTRIBUTES(RANGE_CELL_MM          );
     FUNCTION FA_PIXEL_M10   IS NEW ATTRIBUTES(PIXEL_M10              );
     FUNCTION FA_RULER_M8    IS NEW ATTRIBUTES(RULER_M8               );
     FUNCTION FA_HOURS_M16   IS NEW ATTRIBUTES(HOURS_M16              );
     FUNCTION FA_MILES_M16   IS NEW ATTRIBUTES(MILES_M16              );
     FUNCTION FA_SYMMETRIC_DEGREES_M7
                             IS NEW ATTRIBUTES(SYMMETRIC_DEGREES_M7   );
     FUNCTION FA_NATURAL_DEGREES_M15
                             IS NEW ATTRIBUTES(NATURAL_DEGREES_M15    );
     FUNCTION FA_SYMMETRIC_RADIANS_M16
                             IS NEW ATTRIBUTES(SYMMETRIC_RADIANS_M16  );
     FUNCTION FA_NATURAL_RADIANS_M8
                             IS NEW ATTRIBUTES(NATURAL_RADIANS_M8     );
     FUNCTION FA_ST_MILES_M8 IS NEW ATTRIBUTES(ST_MILES_M8            );
     FUNCTION FA_ST_NATURAL_DEGREES_M11
                             IS NEW ATTRIBUTES(ST_NATURAL_DEGREES_M11 );
     FUNCTION FA_ST_SYMMETRIC_RADIANS_M8
                             IS NEW ATTRIBUTES(ST_SYMMETRIC_RADIANS_M8);

BEGIN

     TEST ("C35A05Q", "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
                      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " &
                      "TYPICAL TYPES, GENERICS");

     CHECK_ATTRIBUTES ("MICRO_ANGLE_ERROR_M15",
                                    FA_MICRO_ANGLE_ERROR_M15, (7, 1) );

     CHECK_ATTRIBUTES ("TRACK_RANGE_M15", FA_TRACK_RANGE_M15, (5, 1) );

     CHECK_ATTRIBUTES ("SECONDS_MM", FA_SECONDS_MM, (4, 5) );

     CHECK_ATTRIBUTES ("RANGE_CELL_MM", FA_RANGE_CELL_MM, (7, 2) );

     CHECK_ATTRIBUTES ("PIXEL_M10", FA_PIXEL_M10, (2, 4) );

     CHECK_ATTRIBUTES ("RULER_M8",  FA_RULER_M8, (3, 2) );

     CHECK_ATTRIBUTES ("HOURS_M16", FA_HOURS_M16, (3, 4) );

     CHECK_ATTRIBUTES ("MILES_M16", FA_MILES_M16, (5, 2) );

     CHECK_ATTRIBUTES ("SYMMETRIC_DEGREES_M7",
                                    FA_SYMMETRIC_DEGREES_M7, (4, 1) );

     CHECK_ATTRIBUTES ("NATURAL_DEGREES_M15",
                                    FA_NATURAL_DEGREES_M15, (4, 2) );

     CHECK_ATTRIBUTES ("SYMMETRIC_RADIANS_M16",
                                    FA_SYMMETRIC_RADIANS_M16, (2, 5) );

     CHECK_ATTRIBUTES ("NATURAL_RADIANS_M8",
                                    FA_NATURAL_RADIANS_M8, (2, 2) );

     CHECK_ATTRIBUTES ("ST_MILES_M8", FA_ST_MILES_M8, (3, 2) );

     CHECK_ATTRIBUTES ("ST_NATURAL_DEGREES_M11",
                                    FA_ST_NATURAL_DEGREES_M11, (4, 1) );

     CHECK_ATTRIBUTES ("ST_SYMMETRIC_RADIANS_M8",
                                   FA_ST_SYMMETRIC_RADIANS_M8, (2, 2) );

     RESULT;

END C35A05Q;
