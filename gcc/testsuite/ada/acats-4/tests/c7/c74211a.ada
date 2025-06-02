-- C74211A.ADA

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
-- CHECK THAT WITHIN THE PACKAGE SPECIFICATION AND BODY, ANY EXPLICIT
-- DECLARATIONS OF OPERATORS AND SUBPROGRAMS HIDE ANY OPERATIONS WHICH
-- ARE IMPLICITLY DECLARED AT THE POINT OF THE FULL DECLARATION,
-- REGARDLESS OF THE ORDER OF OCCURENCE OF THE DECLARATIONS.

-- CHECK THAT IMPLICITLY DECLARED DERIVED SUBPROGRAMS HIDE IMPLICITLY
-- DECLARED PREDEFINED OPERATORS, REGARDLESS OF THE ORDER OF OCCURENCE
-- OF THE DECLARATIONS.

-- DSJ 4/28/83
-- JBG 9/23/83

--   A) EXPLICIT DECLARATION HIDES LATER IMPLICIT DECL OF PREDEFINED OP.
--   B)     "        "         "   LATER     "      "  "  DERIVED OP.
--   C)     "        "         "   EARLIER   "      "  "  PREDEFINED OP.
--   D)     "        "         "   EARLIER   "      "  "  DERIVED OP.

WITH REPORT;
PROCEDURE C74211A IS

     USE REPORT;

BEGIN

     TEST ("C74211A", "CHECK THAT HIDING OF IMPLICITLY DECLARED " &
                      "OPERATORS AND DERIVED SUBPROGRAMS IS DONE " &
                      "CORRECTLY REGARDLESS OF ORDER OF DECL'S");

     DECLARE

          PACKAGE P1 IS 
               TYPE T1 IS RANGE 1 .. 50;
               C1 : CONSTANT T1 := T1(IDENT_INT(2));
               D1 : CONSTANT T1 := C1 + C1;        -- PREDEFINED "+"
               FUNCTION "+" (L, R : T1) RETURN T1; -- C) FOR "+".
               FUNCTION "-" (L, R : T1) RETURN T1; -- C) FOR "-".
               FUNCTION "/" (L, R : T1) RETURN T1; 
          END P1;

          USE P1;

          PACKAGE BODY P1 IS
               A,B : T1 := 3;

               FUNCTION "+" (L, R : T1) RETURN T1 IS
               BEGIN
                    IF L = R THEN
                         RETURN 1;
                    ELSE RETURN 2;
                    END IF;
               END "+";

               FUNCTION "-" (L, R : T1) RETURN T1 IS
               BEGIN
                    IF L = R THEN
                         RETURN 3;
                    ELSE RETURN 4;
                    END IF;
               END "-";

               FUNCTION "/" (L, R : T1) RETURN T1 IS
               BEGIN
                    IF L = R THEN
                         RETURN T1(IDENT_INT(INTEGER(L)));
                    ELSE
                         RETURN T1(IDENT_INT(50));
                    END IF;
               END "/";

          BEGIN
               IF D1 /= 4 THEN
                    FAILED ("WRONG PREDEFINED OPERATION - '+' ");
               END IF;

               IF D1 + C1 /= 2 THEN
                    FAILED ("IMPLICIT '+' NOT HIDDEN BY EXPLICIT '+'");
               END IF;

               IF A + B /= 1 THEN
                    FAILED ("IMPLICIT DECLARATION NOT HIDDEN " &
                            "BY EXPLICIT DECLARATION - '+' ");
               END IF;

               IF A - B /= 3 THEN
                    FAILED ("IMPLICIT DECLARATION NOT HIDDEN " &
                            "BY EXPLICIT DECLARATION - '-' ");
               END IF;

               IF A * B /= 9 THEN
                    FAILED ("WRONG PREDEFINED OPERATION - '*' ");
               END IF;

               IF B / A  /=  T1(IDENT_INT(3)) THEN
                    FAILED ("NOT REDEFINED '/' ");
               END IF;
          END P1;

          PACKAGE P2 IS
               TYPE T2 IS PRIVATE;
               X , Y : CONSTANT T2;
               FUNCTION "+" (L, R : T2) RETURN T2;     -- B)
               FUNCTION "*" (L, R : T2) RETURN T2;     -- A)
          PRIVATE
               TYPE T2 IS NEW T1;                 -- B) +; A) *
               Z : T2 := T2(IDENT_INT(3))/4;      -- Z = 50 USING
                                                  -- DERIVED /
               FUNCTION "/" (L, R : T2) RETURN T2;  -- D) FOR /
               X , Y : CONSTANT T2 := 3;
          END P2;

          PACKAGE BODY P2 IS
               FUNCTION "+" (L, R : T2) RETURN T2 IS
               BEGIN
                    IF L = R THEN
                         RETURN T2(IDENT_INT(5));
                    ELSE RETURN T2(IDENT_INT(6));
                    END IF;
               END "+";

               FUNCTION "*" (L, R : T2) RETURN T2 IS
               BEGIN
                    IF L = R THEN
                         RETURN T2(IDENT_INT(7));
                    ELSE RETURN T2(IDENT_INT(8));
                    END IF;
               END "*";

               FUNCTION "/" (L, R : T2) RETURN T2 IS
               BEGIN
                    IF L = R THEN
                         RETURN T2(IDENT_INT(9));
                    ELSE RETURN T2(IDENT_INT(10));
                    END IF;
               END "/";
          BEGIN
               IF X + Y /= 5 THEN
                         FAILED ("DERIVED SUBPROGRAM NOT HIDDEN BY " &
                                 "EXPLICIT DECLARATION - '+' ");
               END IF;

               IF Y - X /= 3 THEN
                         FAILED ("PREDEFINED OPERATOR NOT HIDDEN BY " &
                                 "DERIVED SUBPROGRAM - '-' ");
               END IF;

               IF X * Y /= 7 THEN
                         FAILED ("PREDEFINED OPERATOR NOT HIDDEN BY " &
                                 "EXPLICIT DECLARATION - '*' ");
               END IF;

               IF Y / X /= T2(IDENT_INT(9)) THEN
                         FAILED ("DERIVED OPERATOR NOT HIDDEN BY " &
                                 "EXPLICIT DECLARATION - '/' ");
               END IF;

               IF Z /= 50 THEN
                    FAILED ("DERIVED OPERATOR HIDDEN PREMATURELY " &
                            " BY REDECLARED OPERATOR");
               END IF;

          END P2;

     BEGIN

          NULL;

     END;

     RESULT;

END C74211A;
