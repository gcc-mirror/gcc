-- C35902D.ADA

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
-- CHECK THAT THE BINARY POINT IN THE MANTISSA OF A FIXED POINT NUMBER
-- CAN LIE OUTSIDE THE MANTISSA (EITHER TO THE LEFT OR TO THE RIGHT).

-- WRG 7/18/86

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C35902D IS

BEGIN

     TEST ("C35902D", "CHECK THAT THE BINARY POINT IN THE MANTISSA " &
                      "OF A FIXED POINT NUMBER CAN LIE OUTSIDE THE " &
                      "MANTISSA (EITHER TO THE LEFT OR TO THE RIGHT)");

     COMMENT ("VALUE OF SYSTEM.MAX_MANTISSA IS" &
              POSITIVE'IMAGE(MAX_MANTISSA) );

  A: DECLARE

          RS : CONSTANT := 2.0;

          TYPE ONE_TO_THE_RIGHT IS
               DELTA RS
               RANGE -(2.0 ** (MAX_MANTISSA+1) ) ..
                       2.0 ** (MAX_MANTISSA+1);
               -- THE BINARY POINT IS ONE PLACE TO THE RIGHT OF THE
               -- LARGEST POSSIBLE MANTISSA.

          R1, R2 : ONE_TO_THE_RIGHT;

     BEGIN

          R1 := RS;
          FOR I IN POSITIVE RANGE 1 .. MAX_MANTISSA - 1 LOOP
               R1 := R1 * IDENT_INT (2);
          END LOOP;
          R2 := R1 - RS;
          R2 := R2 + R1;
          -- AT THIS POINT, R2 SHOULD EQUAL ONE_TO_THE_RIGHT'LARGE.
          R2 := -R2;
          R2 := R2 + (R1 - RS);
          FOR I IN POSITIVE RANGE 1 .. MAX_MANTISSA - 1 LOOP
               R2 := R2 / IDENT_INT (2);
          END LOOP;
          IF R2 /= -RS THEN
               FAILED ("IDENTITY-PRESERVING OPERATIONS ARE FLAKY - A");
          END IF;

     EXCEPTION

          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - A");

     END A;

  B: DECLARE

          LS : CONSTANT := 2.0 ** (-(MAX_MANTISSA+1) );

          TYPE ONE_TO_THE_LEFT IS
               DELTA LS
               RANGE -(2.0 ** (-1) ) ..
                       2.0 ** (-1);
               -- THE BINARY POINT IS ONE PLACE TO THE LEFT OF THE
               -- LARGEST POSSIBLE MANTISSA.

          L1, L2 : ONE_TO_THE_LEFT;

     BEGIN

          L1 := LS;
          FOR I IN POSITIVE RANGE 1 .. MAX_MANTISSA - 1 LOOP
               L1 := L1 * IDENT_INT (2);
          END LOOP;
          L2 := L1 - LS;
          L2 := L2 + L1;
          -- AT THIS POINT, L2 SHOULD EQUAL ONE_TO_THE_LEFT'LARGE.
          L2 := -L2;
          L2 := L2 + (L1 - LS);
          FOR I IN POSITIVE RANGE 1 .. MAX_MANTISSA - 1 LOOP
               L2 := L2 / IDENT_INT (2);
          END LOOP;
          IF L2 /= -LS THEN
               FAILED ("IDENTITY-PRESERVING OPERATIONS ARE FLAKY - B");
          END IF;

     EXCEPTION

          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - B");

     END B;

     RESULT;

END C35902D;
