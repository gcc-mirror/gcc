-- C67002C.ADA

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
-- CHECK THAT ALL OPERATOR SYMBOLS CAN BE USED IN (OVERLOADED)
-- FUNCTION SPECIFICATIONS WITH THE REQUIRED NUMBER OF PARAMETERS.
-- THIS TEST CHECKS FORMAL SUBPROGRAM PARAMETERS.
--   SUBTESTS ARE:
--        (A) THROUGH (P): "=", "AND", "OR", "XOR", "<", "<=",
--             ">", ">=", "&", "*", "/", "MOD", "REM", "**", "+", "-",
--             RESPECTIVELY.  ALL OF THESE HAVE TWO PARAMETERS.
--        (Q), (R), (S), AND (T): "+", "-", "NOT", "ABS", RESPECTIVELY,
--             WITH ONE PARAMETER.

-- CPP 6/26/84

WITH REPORT;  USE REPORT;
PROCEDURE C67002C IS

     FUNCTION TWO_PARAMS (I1, I2 : INTEGER) RETURN CHARACTER IS
     BEGIN
          IF I1 > I2 THEN
               RETURN 'G';
          ELSE RETURN 'L';
          END IF;
     END TWO_PARAMS;

     FUNCTION ONE_PARAM (I1 : INTEGER) RETURN CHARACTER IS
     BEGIN
          IF I1 < IDENT_INT(0) THEN
               RETURN 'N';
          ELSE RETURN 'P';
          END IF;
     END ONE_PARAM;

BEGIN
     TEST ("C67002C", "USE OF OPERATOR SYMBOLS IN " &
           "(OVERLOADED) FUNCTION SPECIFICATIONS");

     -------------------------------------------------

     DECLARE -- (A)

          PACKAGE EQU IS
               TYPE  LP IS LIMITED PRIVATE;
               FUNCTION "=" (LPA, LPB : LP) RETURN BOOLEAN;
          PRIVATE
               TYPE LP IS NEW INTEGER;
          END EQU;
          USE EQU;

          LP1, LP2 : LP;

          PACKAGE BODY EQU IS
               FUNCTION "=" (LPA, LPB : LP) RETURN BOOLEAN IS
               BEGIN
                    RETURN LPA > LPB;
               END "=";
          BEGIN
               LP1 := LP (IDENT_INT (7));
               LP2 := LP (IDENT_INT (8));
          END EQU;

          GENERIC
               WITH FUNCTION "=" (LPA, LPB : LP) RETURN BOOLEAN;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (LP1 = LP2) OR NOT (LP2 = LP1) OR
                  (LP1 = LP1) OR (LP2 /= LP1) THEN
                    FAILED ("OVERLOADING OF ""="" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE EQUAL IS NEW PKG ("=" => EQU."=");

     BEGIN -- (A)
          NULL;
     END; -- (A)

     -------------------------------------------------

     DECLARE -- (B)

          GENERIC
               WITH FUNCTION "AND" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) AND 1) /= 'G' OR
                  (5 AND 10) /= 'L' THEN
                   FAILED ("OVERLOADING OF ""AND"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("AND" => TWO_PARAMS);

     BEGIN -- (B)
          NULL;
     END; -- (B)

     -------------------------------------------------

     DECLARE -- (C)

          GENERIC
               WITH FUNCTION "OR" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) OR 1) /= 'G' OR
                  (5 OR 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""OR"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("OR" => TWO_PARAMS);

     BEGIN -- (C)
          NULL;
     END; -- (C)

     -------------------------------------------------

     DECLARE -- (D)

          GENERIC
               WITH FUNCTION "XOR" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) XOR 1) /= 'G' OR
                  (5 XOR 10) /= 'L' THEN
                   FAILED ("OVERLOADING OF ""XOR"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("XOR" => TWO_PARAMS);

     BEGIN -- (D)
          NULL;
     END; -- (D)

     -------------------------------------------------

     DECLARE -- (E)

          GENERIC
               WITH FUNCTION "<" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) < 1) /= 'G' OR
                  (5 < 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""<"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("<" => TWO_PARAMS);

     BEGIN -- (E)
          NULL;
     END; -- (E)

     -------------------------------------------------

     DECLARE -- (F)

          GENERIC
               WITH FUNCTION "<=" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) <= 1) /= 'G' OR
                  (5 <= 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""<="" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("<=" => TWO_PARAMS);

     BEGIN -- (F)
          NULL;
     END; -- (F)

     -------------------------------------------------

     DECLARE -- (G)

          GENERIC
               WITH FUNCTION ">" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) > 1) /= 'G' OR
                  (5 > 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF "">"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG (">" => TWO_PARAMS);

     BEGIN -- (G)
          NULL;
     END; -- (G)

     -------------------------------------------------

     DECLARE -- (H)

          GENERIC
               WITH FUNCTION ">=" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) >= 1) /= 'G' OR
                  (5 >= 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF "">="" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG (">=" => TWO_PARAMS);

     BEGIN -- (H)
          NULL;
     END; -- (H)

     -------------------------------------------------

     DECLARE -- (I)

          GENERIC
               WITH FUNCTION "&" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) & 1) /= 'G' OR
                  (5 & 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""&"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("&" => TWO_PARAMS);

     BEGIN -- (I)
          NULL;
     END; -- (I)

     -------------------------------------------------

     DECLARE -- (J)

          GENERIC
               WITH FUNCTION "*" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) * 1) /= 'G' OR
                  (5 * 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""*"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("*" => TWO_PARAMS);

     BEGIN -- (J)
          NULL;
     END; -- (J)

     -------------------------------------------------

     DECLARE -- (K)

          GENERIC
               WITH FUNCTION "/" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) / 1) /= 'G' OR
                  (5 / 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""/"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("/" => TWO_PARAMS);

     BEGIN -- (K)
          NULL;
     END; -- (K)

     -------------------------------------------------

     DECLARE -- (L)

          GENERIC
               WITH FUNCTION "MOD" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) MOD 1) /= 'G' OR
                  (5 MOD 10) /= 'L' THEN
                   FAILED ("OVERLOADING OF ""MOD"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("MOD" => TWO_PARAMS);

     BEGIN -- (L)
          NULL;
     END; -- (L)

     -------------------------------------------------

     DECLARE -- (M)

          GENERIC
               WITH FUNCTION "REM" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) REM 1) /= 'G' OR
                  (5 REM 10) /= 'L' THEN
                   FAILED ("OVERLOADING OF ""REM"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("REM" => TWO_PARAMS);

     BEGIN -- (M)
          NULL;
     END; -- (M)

     -------------------------------------------------

     DECLARE -- (N)

          GENERIC
               WITH FUNCTION "**" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) ** 1) /= 'G' OR
                  (5 ** 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""**"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("**" => TWO_PARAMS);

     BEGIN -- (N)
          NULL;
     END; -- (N)

     -------------------------------------------------

     DECLARE -- (O)

          GENERIC
               WITH FUNCTION "+" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) + 1) /= 'G' OR
                  (5 + 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""+"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("+" => TWO_PARAMS);

     BEGIN -- (O)
          NULL;
     END; -- (O)

     -------------------------------------------------

     DECLARE -- (P)

          GENERIC
               WITH FUNCTION "-" (I1, I2 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (IDENT_INT (10) - 1) /= 'G' OR
                  (5 - 10) /= 'L' THEN
                    FAILED ("OVERLOADING OF ""-"" OPERATOR DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("-" => TWO_PARAMS);

     BEGIN -- (P)
          NULL;
     END; -- (P)

     -------------------------------------------------

     DECLARE -- (Q)

          GENERIC
               WITH FUNCTION "+" (I1 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (+ IDENT_INT(25) /= 'P') OR
                  (+ (0-25) /= 'N') THEN
                    FAILED ("OVERLOADING OF ""+"" " &
                            "OPERATOR (ONE OPERAND) DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("+" => ONE_PARAM);

     BEGIN -- (Q)
          NULL;
     END; -- (Q)

     -------------------------------------------------

     DECLARE -- (R)

          GENERIC
               WITH FUNCTION "-" (I1 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (- IDENT_INT(25) /= 'P') OR
                  (- (0-25) /= 'N') THEN
                    FAILED ("OVERLOADING OF ""-"" " &
                            "OPERATOR (ONE OPERAND) DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("-" => ONE_PARAM);

     BEGIN -- (R)
          NULL;
     END; -- (R)

     -------------------------------------------------

     DECLARE -- (S)

          GENERIC
               WITH FUNCTION "NOT" (I1 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (NOT IDENT_INT(25) /= 'P') OR
                  (NOT (0-25) /= 'N') THEN
                    FAILED ("OVERLOADING OF ""NOT"" " &
                            "OPERATOR (ONE OPERAND) DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("NOT" => ONE_PARAM);

     BEGIN -- (S)
          NULL;
     END; -- (S)

     -------------------------------------------------

     DECLARE -- (T)

          GENERIC
               WITH FUNCTION "ABS" (I1 : INTEGER) RETURN CHARACTER;
          PACKAGE PKG IS
          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               IF (ABS IDENT_INT(25) /= 'P') OR
                  (ABS (0-25) /= 'N') THEN
                    FAILED ("OVERLOADING OF ""ABS"" " &
                            "OPERATOR (ONE OPERAND) DEFECTIVE");
               END IF;
          END PKG;

          PACKAGE PACK IS NEW PKG ("ABS" => ONE_PARAM);

     BEGIN -- (T)
          NULL;
     END; -- (T)

     -------------------------------------------------

     RESULT;
END C67002C;
  
