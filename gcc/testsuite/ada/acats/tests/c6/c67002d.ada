-- C67002D.ADA

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
-- THIS TEST CHECKS GENERIC INSTANTIATIONS FOR THESE FUNCTIONS.
--   SUBTESTS ARE:
--        (A) THROUGH (P): "=", "AND", "OR", "XOR", "<", "<=",
--             ">", ">=", "&", "*", "/", "MOD", "REM", "**", "+", "-",
--             RESPECTIVELY.  ALL OF THESE HAVE TWO PARAMETERS.
--        (Q), (R), (S), AND (T): "+", "-", "NOT", "ABS", RESPECTIVELY,
--             WITH ONE PARAMETER.

-- CPP 6/25/84

WITH REPORT;  USE REPORT;
PROCEDURE C67002D IS

     GENERIC
          TYPE ELEMENT IS (<>);
     FUNCTION TWO_PARAMS (I1, I2 : ELEMENT) RETURN CHARACTER;
     FUNCTION TWO_PARAMS (I1, I2 : ELEMENT) RETURN CHARACTER IS
     BEGIN
          IF I1 > I2 THEN
               RETURN 'G';
          ELSE RETURN 'L';
          END IF;
     END TWO_PARAMS;

     GENERIC
          TYPE ELEMENT IS (<>);
     FUNCTION ONE_PARAM (I1 : ELEMENT) RETURN CHARACTER;
     FUNCTION ONE_PARAM (I1 : ELEMENT) RETURN CHARACTER IS
     BEGIN
          IF I1 < ELEMENT'VAL(IDENT_INT(0)) THEN
               RETURN 'N';
          ELSE RETURN 'P';
          END IF;
     END ONE_PARAM;
     
BEGIN
     TEST ("C67002D", "USE OF OPERATOR SYMBOLS IN " &
           "(OVERLOADED) FUNCTION SPECIFICATIONS");

     -------------------------------------------------

     DECLARE -- (A)
          GENERIC
               TYPE LP IS LIMITED PRIVATE;
               WITH FUNCTION ">" (L, R : LP) RETURN BOOLEAN IS <>;
          PACKAGE PKG IS
               LP1, LP2 : LP;
               FUNCTION "=" (LPA, LPB : LP) RETURN BOOLEAN;
          END PKG;

          PACKAGE BODY PKG IS
               FUNCTION "=" (LPA, LPB : LP) RETURN BOOLEAN IS
               BEGIN
                    RETURN LPA > LPB;
               END "=";
          END PKG;

     BEGIN -- (A)
          DECLARE
               PACKAGE PACK IS NEW PKG (LP => INTEGER);
               USE PACK;
               FUNCTION "=" (L, R : INTEGER) RETURN BOOLEAN
                    RENAMES PACK."=";
          BEGIN
               LP1 := IDENT_INT(7);
               LP2 := IDENT_INT(8);
               IF (LP1 = LP2) OR NOT (LP2 = LP1) OR
                  (LP1 = LP1) OR (LP2 /= LP1) THEN
                    FAILED ("OVERLOADING OF ""="" OPERATOR DEFECTIVE");
               END IF;
          END;
     END; -- (A)

     -------------------------------------------------

     DECLARE -- (B)
          FUNCTION "AND" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (B)
          IF (IDENT_INT (10) AND 1) /= 'G' OR
             (5 AND 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""AND"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (B)

     -------------------------------------------------

     DECLARE -- (C)
          FUNCTION "OR" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (C)
          IF (IDENT_INT (10) OR 1) /= 'G' OR
             (5 OR 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""OR"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (C)

     -------------------------------------------------

     DECLARE -- (D)
          FUNCTION "XOR" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (D)
          IF (IDENT_INT (10) XOR 1) /= 'G' OR
             (5 XOR 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""XOR"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (D)

     -------------------------------------------------

     DECLARE -- (E)
          FUNCTION "<" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (E)
          IF (IDENT_INT (10) < 1) /= 'G' OR
             (5 < 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""<"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (E)

     -------------------------------------------------

     DECLARE -- (F)
          FUNCTION "<=" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (F)
          IF (IDENT_INT (10) <= 1) /= 'G' OR
             (5 <= 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""<="" OPERATOR DEFECTIVE");
          END IF;
     END; -- (F)

     -------------------------------------------------

     DECLARE -- (G)
          FUNCTION ">" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (G)
          IF (IDENT_INT (10) > 1) /= 'G' OR
             (5 > 10) /= 'L' THEN
               FAILED ("OVERLOADING OF "">"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (G)

     -------------------------------------------------

     DECLARE -- (H)
          FUNCTION ">=" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (H)
          IF (IDENT_INT (10) >= 1) /= 'G' OR
             (5 >= 10) /= 'L' THEN
               FAILED ("OVERLOADING OF "">="" OPERATOR DEFECTIVE");
          END IF;
     END; -- (H)

     -------------------------------------------------

     DECLARE -- (I)
          FUNCTION "&" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (I)
          IF (IDENT_INT (10) & 1) /= 'G' OR
             (5 & 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""&"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (I)

     -------------------------------------------------

     DECLARE -- (J)
          FUNCTION "*" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (J)
          IF (IDENT_INT (10) * 1) /= 'G' OR
             (5 * 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""*"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (J)

     -------------------------------------------------

     DECLARE -- (K)
          FUNCTION "/" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (K)
          IF (IDENT_INT (10) / 1) /= 'G' OR
             (5 / 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""/"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (K)

     -------------------------------------------------

     DECLARE -- (L)
          FUNCTION "MOD" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (L)
          IF (IDENT_INT (10) MOD 1) /= 'G' OR
             (5 MOD 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""MOD"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (L)

     -------------------------------------------------

     DECLARE -- (M)
          FUNCTION "REM" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (M)
          IF (IDENT_INT (10) REM 1) /= 'G' OR
             (5 REM 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""REM"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (M)

     -------------------------------------------------

     DECLARE -- (N)
          FUNCTION "**" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (N)
          IF (IDENT_INT (10) ** 1) /= 'G' OR
             (5 ** 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""**"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (N)

     -------------------------------------------------

     DECLARE -- (O)
          FUNCTION "+" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (O)
          IF (IDENT_INT (10) + 1) /= 'G' OR
             (5 + 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""+"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (O)

     -------------------------------------------------

     DECLARE -- (P)
          FUNCTION "-" IS NEW TWO_PARAMS
               (ELEMENT => INTEGER);

     BEGIN -- (P)
          IF (IDENT_INT (10) - 1) /= 'G' OR
             (5 - 10) /= 'L' THEN
               FAILED ("OVERLOADING OF ""-"" OPERATOR DEFECTIVE");
          END IF;
     END; -- (P)

     -------------------------------------------------

     DECLARE -- (Q)
          FUNCTION "+" IS NEW ONE_PARAM
               (ELEMENT => INTEGER);

     BEGIN -- (Q)
          IF (+ IDENT_INT(25) /= 'P') OR
             (+ (0-25) /= 'N') THEN
               FAILED ("OVERLOADING OF ""+"" " &
                       "OPERATOR (ONE OPERAND) DEFECTIVE");
          END IF;
     END; -- (Q)

     -------------------------------------------------

     DECLARE -- (R)
          FUNCTION "-" IS NEW ONE_PARAM
               (ELEMENT => INTEGER);

     BEGIN -- (R)
          IF (- IDENT_INT(25) /= 'P') OR
             (- (0-25) /= 'N') THEN
               FAILED ("OVERLOADING OF ""-"" " &
                       "OPERATOR (ONE OPERAND) DEFECTIVE");
          END IF;
     END; -- (R)

     -------------------------------------------------

     DECLARE -- (S)
          FUNCTION "NOT" IS NEW ONE_PARAM
               (ELEMENT => INTEGER);

     BEGIN -- (S)
          IF (NOT IDENT_INT(25) /= 'P') OR
             (NOT (0-25) /= 'N') THEN
               FAILED ("OVERLOADING OF ""NOT"" " &
                       "OPERATOR (ONE OPERAND) DEFECTIVE");
          END IF;
     END; -- (S)

     -------------------------------------------------

     DECLARE -- (T)
          FUNCTION "ABS" IS NEW ONE_PARAM
               (ELEMENT => INTEGER);

     BEGIN -- (T)
          IF (ABS IDENT_INT(25) /= 'P') OR
             (ABS (0-25) /= 'N') THEN
               FAILED ("OVERLOADING OF ""ABS"" " &
                       "OPERATOR (ONE OPERAND) DEFECTIVE");
          END IF;
     END; -- (T)

     -------------------------------------------------

     RESULT;
END C67002D;
