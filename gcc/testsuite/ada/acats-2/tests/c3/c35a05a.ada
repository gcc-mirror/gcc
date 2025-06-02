-- C35A05A.ADA

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

-- CASE A: BASIC TYPES THAT FIT THE CHARACTERISTICS OF DURATION'BASE.

-- WRG 8/8/86

WITH REPORT; USE REPORT;
PROCEDURE C35A05A IS

     -- THE NAME OF EACH TYPE OR SUBTYPE ENDS WITH THAT TYPE'S
     -- 'MANTISSA VALUE.

     TYPE LEFT_OUT_M1       IS DELTA 0.25  RANGE -0.5 .. 0.5;
     TYPE LEFT_EDGE_M1      IS DELTA 0.5   RANGE -1.0 .. 1.0;
     TYPE RIGHT_EDGE_M1     IS DELTA 1.0   RANGE -2.0 .. 2.0;
     TYPE RIGHT_OUT_M1      IS DELTA 2.0   RANGE -4.0 .. 4.0;
     TYPE MIDDLE_M2         IS DELTA 0.5   RANGE -2.0 .. 2.0;
     TYPE MIDDLE_M3         IS DELTA 0.5   RANGE  0.0 .. 2.5;
     TYPE MIDDLE_M15        IS DELTA 2.0 **(-6) RANGE  -512.0 ..  512.0;
     TYPE MIDDLE_M16        IS DELTA 2.0 **(-6) RANGE -1024.0 .. 1024.0;
     TYPE LIKE_DURATION_M23 IS DELTA 0.020 RANGE -86_400.0 .. 86_400.0;
     TYPE DECIMAL_M18       IS DELTA 0.1   RANGE -10_000.0 .. 10_000.0;
     TYPE DECIMAL_M4        IS DELTA 100.0 RANGE   -1000.0 ..   1000.0;
     TYPE DECIMAL_M11       IS DELTA 0.09999 RANGE  -100.0 ..    100.0;
     TYPE DECIMAL2_M18      IS DELTA 0.1   RANGE   -9999.0 ..   9999.0;

     -------------------------------------------------------------------

     SUBTYPE ST_LEFT_EDGE_M6 IS MIDDLE_M15
          DELTA 2.0 ** (-6) RANGE IDENT_INT (1) * (-1.0) .. 1.0;
     SUBTYPE ST_MIDDLE_M14   IS MIDDLE_M16
          DELTA 2.0 ** (-5) RANGE -512.0 .. IDENT_INT (1) * 512.0;
     SUBTYPE ST_MIDDLE_M2    IS LIKE_DURATION_M23
          DELTA 0.5 RANGE -2.0 .. 2.0;
     SUBTYPE ST_MIDDLE_M3    IS LIKE_DURATION_M23
          DELTA 0.5 RANGE  0.0 .. 2.5;
     SUBTYPE ST_DECIMAL_M7   IS DECIMAL_M18
          DELTA  10.0 RANGE -1000.0 .. 1000.0;
     SUBTYPE ST_DECIMAL_M3   IS DECIMAL_M4
          DELTA 100.0 RANGE  -500.0 ..  500.0;

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

     TEST ("C35A05A", "CHECK THAT FOR FIXED POINT TYPES THE FORE AND " &
                      "AFT ATTRIBUTES YIELD THE CORRECT VALUES - " &
                      "BASIC TYPES");

     CHECK_FORE_AND_AFT ("LEFT_OUT_M1",       LEFT_OUT_M1'FORE, 2,
                                              LEFT_OUT_M1'AFT,  1);

     CHECK_FORE_AND_AFT ("LEFT_EDGE_M1",      LEFT_EDGE_M1'FORE, 2,
                                              LEFT_EDGE_M1'AFT,  1);

     CHECK_FORE_AND_AFT ("RIGHT_EDGE_M1",     RIGHT_EDGE_M1'FORE, 2,
                                              RIGHT_EDGE_M1'AFT,  1);

     CHECK_FORE_AND_AFT ("RIGHT_OUT_M1",      RIGHT_OUT_M1'FORE, 2,
                                              RIGHT_OUT_M1'AFT,  1);

     CHECK_FORE_AND_AFT ("MIDDLE_M2",         MIDDLE_M2'FORE, 2,
                                              MIDDLE_M2'AFT,  1);

     CHECK_FORE_AND_AFT ("MIDDLE_M3",         MIDDLE_M3'FORE, 2,
                                              MIDDLE_M3'AFT,  1);

     CHECK_FORE_AND_AFT ("MIDDLE_M15",        MIDDLE_M15'FORE, 4,
                                              MIDDLE_M15'AFT,  2);

     CHECK_FORE_AND_AFT ("MIDDLE_M16",        MIDDLE_M16'FORE, 5,
                                              MIDDLE_M16'AFT,  2);

     CHECK_FORE_AND_AFT ("LIKE_DURATION_M23", LIKE_DURATION_M23'FORE, 6,
                                              LIKE_DURATION_M23'AFT, 2);

     CHECK_FORE_AND_AFT ("DECIMAL_M18",       DECIMAL_M18'FORE, 6,
                                              DECIMAL_M18'AFT,  1);

     IF DECIMAL_M4'FORE /= 5 AND DECIMAL_M4'FORE /= 4 THEN
          FAILED ("DECIMAL_M4'FORE =" &
                  INTEGER'IMAGE(DECIMAL_M4'FORE) );
     END IF;
     IF DECIMAL_M4'AFT /= 1 THEN
          FAILED ("DECIMAL_M4'AFT  =" &
                  INTEGER'IMAGE(DECIMAL_M4'AFT) );
     END IF;

     CHECK_FORE_AND_AFT ("DECIMAL_M11",       DECIMAL_M11'FORE, 4,
                                              DECIMAL_M11'AFT,  2);

     CHECK_FORE_AND_AFT ("DECIMAL2_M18",      DECIMAL2_M18'FORE, 5,
                                              DECIMAL2_M18'AFT,  1);

     CHECK_FORE_AND_AFT ("ST_LEFT_EDGE_M6",   ST_LEFT_EDGE_M6'FORE, 2,
                                              ST_LEFT_EDGE_M6'AFT,  2);

     CHECK_FORE_AND_AFT ("ST_MIDDLE_M14",     ST_MIDDLE_M14'FORE, 4,
                                              ST_MIDDLE_M14'AFT,  2);

     CHECK_FORE_AND_AFT ("ST_MIDDLE_M2",      ST_MIDDLE_M2'FORE, 2,
                                              ST_MIDDLE_M2'AFT,  1);

     CHECK_FORE_AND_AFT ("ST_MIDDLE_M3",      ST_MIDDLE_M3'FORE, 2,
                                              ST_MIDDLE_M3'AFT,  1);

     CHECK_FORE_AND_AFT ("ST_DECIMAL_M7",     ST_DECIMAL_M7'FORE, 5,
                                              ST_DECIMAL_M7'AFT,  1);

     CHECK_FORE_AND_AFT ("ST_DECIMAL_M3",     ST_DECIMAL_M3'FORE, 4,
                                              ST_DECIMAL_M3'AFT,  1);

     RESULT;

END C35A05A;
