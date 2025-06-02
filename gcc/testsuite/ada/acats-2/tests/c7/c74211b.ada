-- C74211B.ADA

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
-- CHECK THAT IMPLICITLY DECLARED INEQUALITY WHICH ACCOMPANIES AN
-- EXPLICIT DECLARATION OF EQUALITY HIDES OTHER IMPLICITLY DECLARED
-- HOMOGRAPHS, AND THAT DERIVED INEQUALITY HIDES PREDEFINED INEQUALITY.

-- DSJ 4/29/83
-- JBG 9/23/83

WITH REPORT;
PROCEDURE C74211B IS

     USE REPORT;

BEGIN

     TEST( "C74211B", "CHECK THAT HIDING OF IMPLICITLY DECLARED " &
                      "OPERATORS AND DERIVED SUBPROGRAMS IS DONE " &
                      "CORRECTLY REGARDLESS OF ORDER OF DECL'S");

     DECLARE

          PACKAGE P1 IS 
               TYPE LT1 IS LIMITED PRIVATE;
               FUNCTION "="(L, R : LT1) RETURN BOOLEAN;
               FUNCTION LT1_VALUE_2 RETURN LT1;
               FUNCTION LT1_VALUE_4 RETURN LT1;
               TYPE LT2 IS LIMITED PRIVATE;
          PRIVATE
               TYPE LT1 IS RANGE 1 .. 10;
               TYPE LT2 IS RANGE 1 .. 10;
          END P1;

          USE P1;

          PACKAGE P2 IS
               TYPE LT3 IS LIMITED PRIVATE;
               TYPE LT4 IS NEW LT1;
          PRIVATE
               FUNCTION "=" (L, R : LT3) RETURN BOOLEAN;
               TYPE LT3 IS NEW LT1;
          END P2;

          USE P2;

          PACKAGE BODY P1 IS
               A , B : CONSTANT LT1 := 4;
               C , D : CONSTANT LT2 := 6;

               FUNCTION "=" (L, R : LT1) RETURN BOOLEAN IS
               BEGIN
                    RETURN INTEGER(L) /= INTEGER(R);
               END "=";

               FUNCTION LT1_VALUE_2 RETURN LT1 IS
               BEGIN
                    RETURN 2;
               END LT1_VALUE_2;

               FUNCTION LT1_VALUE_4 RETURN LT1 IS
               BEGIN
                    RETURN 4;
               END LT1_VALUE_4;

          BEGIN
               IF A = B THEN
                    FAILED ("PREDEFINED EQUALITY NOT HIDDEN BY " &
                            "EXPLICIT DECLARATION - LT1");
               END IF;

               IF C /= D  THEN
                    FAILED ("WRONG PREDEFINED OPERATION - T2");
               END IF;
          END P1;

          PACKAGE BODY P2 IS
               FUNCTION U RETURN LT3 IS
               BEGIN
                    RETURN LT1_VALUE_2;
               END U;

               FUNCTION V RETURN LT3 IS
               BEGIN
                    RETURN LT1_VALUE_4;
               END V;

               FUNCTION W RETURN LT4 IS
               BEGIN
                    RETURN LT1_VALUE_2;
               END W;

               FUNCTION X RETURN LT4 IS
               BEGIN
                    RETURN LT1_VALUE_4;
               END X;

               FUNCTION "=" (L, R : LT3) RETURN BOOLEAN IS
               BEGIN
                    RETURN NOT (LT1(L) = LT1(R));
               END "=";

          BEGIN
               IF NOT (U /= V) THEN
                    FAILED ("DERIVED SUBPROGRAM NOT HIDDEN BY " &
                            "IMPLICITLY DECLARED INEQUALITY " &
                            "FROM EXPLICITLY DECLARED EQUALITY"); 
               END IF;

               IF NOT (LT3(W) = U) THEN
                    FAILED ("DERIVED SUBPROGRAM NOT HIDDEN BY " &
                            "EXPLICIT DECLARATION - '=' ");
               END IF;

               IF W /= X THEN
                    FAILED ("PREDEFINED OPERATOR NOT HIDDEN BY " &
                            "DERIVED SUBPROGRAM - '/=' ");
               END IF;

               IF NOT ( X = W ) THEN
                    FAILED ("PREDEFINED OPERATOR NOT HIDDEN BY " &
                            "DERIVED SUBPROGRAM - '=' ");
               END IF;

          END P2;

     BEGIN

          NULL;

     END;

     RESULT;

END C74211B;
