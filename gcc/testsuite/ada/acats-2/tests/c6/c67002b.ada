-- C67002B.ADA

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
-- CHECK THAT OPERATOR SYMBOLS CAN BE USED IN (OVERLOADED)
--   FUNCTION SPECIFICATIONS WITH THE REQUIRED NUMBER OF PARAMETERS.
--   THIS TEST CHECKS THE CASE OF CERTAIN OPERATOR SYMBOLS.
--   SUBTESTS ARE:
--        (A) THROUGH (E): "AND", "OR", "XOR", "MOD", "REM"
--            RESPECTIVELY.  ALL OF THESE HAVE TWO PARAMETERS.
--        (F) AND (G): "NOT" AND "ABS", RESPECTIVELY,
--             WITH ONE PARAMETER.

-- CPP 6/26/84

WITH REPORT;
PROCEDURE C67002B IS

     USE REPORT;

BEGIN
     TEST ("C67002B", "USE OF OPERATOR SYMBOLS IN " &
           "(OVERLOADED) FUNCTION SPECIFICATIONS");

     -------------------------------------------------

     DECLARE -- (A)
          FUNCTION "And" (I1, I2 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 > I2 THEN
                    RETURN 'G';
               ELSE RETURN 'L';
               END IF;
          END "And";

     BEGIN -- (A)
          IF (IDENT_INT (10) AND 1) /= 'G' OR
             (5 AnD 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""And"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (A)

     -------------------------------------------------

     DECLARE -- (B)
          FUNCTION "or" (I1, I2 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 > I2 THEN
                    RETURN 'G';
               ELSE RETURN 'L';
               END IF;
          END "or";

     BEGIN -- (B)
          IF (IDENT_INT (10) Or 1) /= 'G' OR
             (5 OR 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""or"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (B)

     -------------------------------------------------

     DECLARE -- (C)
          FUNCTION "xOR" (I1, I2 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 > I2 THEN
                    RETURN 'G';
               ELSE RETURN 'L';
               END IF;
          END "xOR";

     BEGIN -- (C)
          IF (IDENT_INT (10) XoR 1) /= 'G' OR
             (5 xOR 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""xOR"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (C)

     -------------------------------------------------

     DECLARE -- (D)
          FUNCTION "mOd" (I1, I2 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 > I2 THEN
                    RETURN 'G';
               ELSE RETURN 'L';
               END IF;
          END "mOd";

     BEGIN -- (D)
          IF (IDENT_INT (10) MoD 1) /= 'G' OR
             (5 moD 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""mOd"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (D)

     -------------------------------------------------

     DECLARE -- (E)
          FUNCTION "REM" (I1, I2 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 > I2 THEN
                    RETURN 'G';
               ELSE RETURN 'L';
               END IF;
          END "REM";

     BEGIN -- (E)
          IF (IDENT_INT (10) rem 1) /= 'G' OR
             (5 Rem 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""REM"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (E)

     -------------------------------------------------

     DECLARE -- (F)
          FUNCTION "NOT" (I1 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 < IDENT_INT (0) THEN
                    RETURN 'N';
               ELSE RETURN 'P';
               END IF;
          END "NOT";

     BEGIN -- (F)
          IF (Not IDENT_INT(25) /= 'P') OR
             (noT (0-25) /= 'N') THEN
               FAILED ("OVERLOADING OF ""NOT"" " &
                       "OPERATOR (ONE OPERAND) DEFECTIVE");
          END IF;
     END; -- (F)

     -------------------------------------------------

     DECLARE -- (G)
          FUNCTION "ABS" (I1 : INTEGER) RETURN CHARACTER IS
          BEGIN
               IF I1 < IDENT_INT (0) THEN
                    RETURN 'N';
               ELSE RETURN 'P';
               END IF;
          END "ABS";

     BEGIN -- (G)
          IF (abs IDENT_INT(25) /= 'P') OR
             (Abs (0-25) /= 'N') THEN
               FAILED ("OVERLOADING OF ""ABS"" " &
                       "OPERATOR (ONE OPERAND) DEFECTIVE");
          END IF;
     END; -- (T)

     -------------------------------------------------

     RESULT;
END C67002B;
