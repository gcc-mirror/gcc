-- C87B39A.ADA

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
-- CHECK THAT:

--   A) AN OVERLOADED CALL CAN BE RESOLVED BECAUSE AN ALLOCATOR RETURNS
--      AN ACCESS TYPE WHOSE DESIGNATED TYPE IS THE TYPE REFERRED TO IN
--      THE ALLOCATOR.
--
--   B) IF THE NAME OF THE DESIGNATED TYPE IN AN ALLOCATOR DOES NOT
--      UNIQUELY DETERMINE THE ACCESS TYPE OF AN ALLOCATOR, THE CONTEXT
--      MUST DETERMINE THE TYPE.

-- JBG 1/30/84

WITH REPORT; USE REPORT;
PROCEDURE C87B39A IS

     TYPE S IS (M, F);
     TYPE R (D : S) IS
          RECORD NULL; END RECORD;
     SUBTYPE M1 IS R(M);
     SUBTYPE M2 IS R(M);

     TYPE ACC_M1 IS ACCESS M1;
     TYPE ACC_M2 IS ACCESS M2;
     TYPE ACC_BOOL IS ACCESS BOOLEAN;
     TYPE ACC_ACC_M1 IS ACCESS ACC_M1;
     
     TYPE WHICH IS (IS_M1, IS_M2, IS_BOOL);

     PROCEDURE P (X : ACC_M1; RESOLUTION : WHICH) IS
     BEGIN
          IF RESOLUTION /= IS_M1 THEN
               FAILED ("INCORRECT RESOLUTION -- ACC_M1");
          END IF;
     END P;    -- ACC_M1

     PROCEDURE P (X : ACC_M2; RESOLUTION : WHICH) IS
     BEGIN
          IF RESOLUTION /= IS_M2 THEN
               FAILED ("INCORRECT RESOLUTION -- ACC_M2");
          END IF;
     END P;    -- ACC_M2

     PROCEDURE P (X : ACC_BOOL; RESOLUTION : WHICH) IS
     BEGIN
          IF RESOLUTION /= IS_BOOL THEN
               FAILED ("INCORRECT RESOLUTION -- ACC_BOOL");
          END IF;
     END P;    -- ACC_BOOL

     PROCEDURE P (X : ACC_ACC_M1; RESOLUTION : WHICH) IS
     BEGIN
          FAILED ("INCORRECT RESOLUTION -- ACC_ACC_M1");
     END P;    -- ACC_ACC_M1

     PROCEDURE Q (X : ACC_M1) IS
     BEGIN
          NULL;
     END Q;    -- ACC_M1

     PROCEDURE Q (X : ACC_BOOL) IS
     BEGIN
          FAILED ("INCORRECT RESOLUTION -- ACC_BOOL: Q");
     END Q;    -- ACC_BOOL

BEGIN

     TEST ("C87B39A", "OVERLOADING RESOLUTION FOR ALLOCATORS");

     P (ACC_M1'(NEW R(M)), IS_M1);    -- B

     P (ACC_M2'(NEW M1), IS_M2);      -- B

     P (NEW BOOLEAN'(TRUE), IS_BOOL); -- A

     Q (NEW M2);              -- A
     Q (NEW M1);              -- A
     Q (NEW R(M));            -- A
     Q (NEW R'(D => M));      -- A

     RESULT;

END C87B39A;
