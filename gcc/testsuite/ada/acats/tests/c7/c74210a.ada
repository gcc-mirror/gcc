-- C74210A.ADA

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
-- CHECK THAT OPERATOR SYMBOLS OVERLOADED IN A PACKAGE ARE
--   USED AND DERIVED IN PREFERENCE TO THOSE OF THE PARENT OF A DERIVED
--   PRIVATE TYPE. 

-- CHECK THAT OPERATOR DEFINITIONS FOR A PRIVATE TYPE MAY BE
--   OVERLOADED OUTSIDE THE PACKAGE.

-- CHECK THAT EQUALITY CAN BE DEFINED FOR LIMITED TYPES AND COMPOSITE
--   TYPES WITH LIMITED COMPONENTS. 

-- DAT 5/11/81

WITH REPORT; USE REPORT;

PROCEDURE C74210A IS
BEGIN
     TEST ("C74210A", "OVERLOADED OPERATORS FOR PRIVATE TYPES");

     DECLARE
          PACKAGE P IS
               TYPE T IS PRIVATE;
               FUNCTION "+" (X, Y : T) RETURN T;
               ONE, TWO : CONSTANT T;

               TYPE L IS LIMITED PRIVATE;
               TYPE A IS ARRAY (0 .. 0) OF L;
               TYPE R IS RECORD
                    C : L;
               END RECORD;
               FUNCTION "=" (X, Y : L) RETURN BOOLEAN;
          PRIVATE
               TYPE T IS NEW INTEGER;
               ONE : CONSTANT T := T(IDENT_INT(1));
               TWO : CONSTANT T := T(IDENT_INT(2));
               TYPE L IS (ENUM);
          END P;
          USE P;

          VR : R;
          VA : A;

          PACKAGE BODY P IS
               FUNCTION "+" (X, Y : T) RETURN T IS
               BEGIN
                    RETURN 1;
               END "+";

               FUNCTION "=" (X, Y : L) RETURN BOOLEAN IS
               BEGIN
                    RETURN IDENT_BOOL(FALSE);
               END "=";
          BEGIN
               VR := (C => ENUM);
               VA := (0 => VR.C);
          END P;
     BEGIN
          IF ONE + TWO /= ONE THEN
               FAILED ("WRONG ""+"" OPERATOR");
          END IF;

          DECLARE
               TYPE NEW_T IS NEW T;

               FUNCTION "=" (X, Y : A) RETURN BOOLEAN;
               FUNCTION "=" (X, Y : R) RETURN BOOLEAN;

               FUNCTION "+" (X, Y : T) RETURN T IS
               BEGIN
                    RETURN TWO;
               END "+";

               FUNCTION "=" (X, Y : A) RETURN BOOLEAN IS
               BEGIN
                    RETURN X(0) = Y(0);
               END "=";

               FUNCTION "=" (X, Y : R) RETURN BOOLEAN IS
               BEGIN
                    RETURN X.C = Y.C;
               END "=";
          BEGIN
               IF ONE + TWO /= TWO THEN
                    FAILED ("WRONG DERIVED ""+"" OPERATOR");
               END IF;

               IF VR = VR OR VA = VA THEN
                    FAILED ("CANNOT OVERLOAD ""="" CORRECTLY");
               END IF;
          END;
     END;

     RESULT;
END C74210A;
