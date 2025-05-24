-- C45672A.ADA

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
-- CHECK THAT "NOT" YIELDS THE CORRECT RESULTS WHEN APPLIED TO
-- ONE-DIMENSIONAL BOOLEAN ARRAYS.

-- JWC 11/15/85

WITH REPORT;USE REPORT;

PROCEDURE C45672A IS
BEGIN

     TEST ("C45672A", "CHECK THE UNARY OPERATOR 'NOT' APPLIED TO " &
                      "ONE-DIMENSIONAL BOOLEAN ARRAYS");

     DECLARE

          TYPE ARR1 IS ARRAY (INTEGER RANGE 1 .. 4) OF BOOLEAN;
          TYPE ARR2 IS ARRAY (INTEGER RANGE 1 .. 40) OF BOOLEAN;
          TYPE ARR3 IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
          TYPE ARR4 IS ARRAY (INTEGER RANGE 1 .. 4) OF BOOLEAN;
          TYPE ARR5 IS ARRAY (INTEGER RANGE 1 .. 40) OF BOOLEAN;

          PRAGMA PACK (ARR4);
          PRAGMA PACK (ARR5);

          A1 : ARR1 := ARR1'(1 | 3 => TRUE, OTHERS => FALSE);
          A2 : ARR2 := ARR2'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => TRUE,
                             OTHERS => FALSE);
          A3 : ARR3(IDENT_INT(3) .. IDENT_INT(4)) := ARR3'(TRUE, FALSE);
          A4 : ARR4 := ARR4'(1 | 3 => TRUE, OTHERS => FALSE);
          A5 : ARR5 := ARR5'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37 => TRUE,
                             OTHERS => FALSE);
          A6 : ARR3 (IDENT_INT(9) .. IDENT_INT(7));

          PROCEDURE P (A : ARR3; F : INTEGER; L : INTEGER) IS
          BEGIN
               IF A'FIRST /= F OR A'LAST /= L THEN
                    FAILED ("'NOT' YIELDED THE WRONG BOUNDS");
               END IF;
          END P;

     BEGIN

          P (NOT A3, 3, 4);
          P (NOT A6, 9, 7);

          IF NOT A1 /= ARR1'(1 | 3 => FALSE, OTHERS => TRUE) THEN
               FAILED ("WRONG RESULT WHEN 'NOT' APPLIED " &
                       "TO SMALL ARRAY");
          END IF;

          IF NOT A2 /= ARR2'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37
                             => FALSE, OTHERS => TRUE) THEN
               FAILED ("WRONG RESULT WHEN 'NOT' APPLIED " &
                       "TO LARGE ARRAY");
          END IF;

          IF NOT A4 /= ARR4'(1 | 3 => FALSE, OTHERS => TRUE) THEN
               FAILED ("WRONG RESULT WHEN 'NOT' APPLIED " &
                       "TO SMALL PACKED ARRAY");
          END IF;

          IF NOT A5 /= ARR5'(1 | 14 .. 18 | 30 .. 33 | 35 .. 37
                             => FALSE, OTHERS => TRUE) THEN
               FAILED ("WRONG RESULT WHEN 'NOT' APPLIED " &
                       "TO LARGE PACKED ARRAY");
          END IF;

          IF "NOT" (RIGHT => A1) /= ARR1'(1 | 3 => FALSE,
                                          OTHERS => TRUE) THEN
               FAILED ("WRONG RESULT WHEN 'NOT' APPLIED " &
                       "TO SMALL ARRAY USING NAMED NOTATION");
          END IF;

          IF "NOT" (RIGHT => A5) /= ARR5'(1 | 14 .. 18 | 30 .. 33 |
                                          35 .. 37 => FALSE,
                                          OTHERS => TRUE) THEN
               FAILED ("WRONG RESULT WHEN 'NOT' APPLIED TO LARGE " &
                       "PACKED ARRAY USING NAMED NOTATION");
          END IF;

     END;

     RESULT;

END C45672A;
