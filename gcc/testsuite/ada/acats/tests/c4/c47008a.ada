-- C47008A.ADA

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
-- OBJECTIVE:
--     WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES A
--     CONSTRAINED RECORD, PRIVATE, OR LIMITED PRIVATE TYPE, CHECK THAT
--     CONSTRAINT_ERROR IS RAISED WHEN THE DISCRIMINANTS OF THE OPERAND
--     DO NOT EQUAL THOSE OF THE TYPE MARK.

-- HISTORY:
--     RJW 07/23/86
--     DWC 07/24/87  CHANGED CODE TO TEST FOR FIRST DISCRIMINANT
--                   AND LAST DISCRIMINANT MISMATCH.

WITH REPORT; USE REPORT;
PROCEDURE C47008A IS

     TYPE GENDER IS (MALE, FEMALE, NEUTER);

     FUNCTION IDENT (G : GENDER) RETURN GENDER IS
     BEGIN
          RETURN GENDER'VAL (IDENT_INT (GENDER'POS (G)));
     END IDENT;

BEGIN

     TEST( "C47008A", "WHEN THE TYPE MARK IN A QUALIFIED " &
                      "EXPRESSION DENOTES A CONSTRAINED RECORD, " &
                      "PRIVATE, OR LIMITED PRIVATE TYPE, CHECK " &
                      "THAT CONSTRAINT_ERROR IS RAISED WHEN THE " &
                      "DISCRIMANTS OF THE OPERAND DO NOT EQUAL " &
                      "THOSE OF THE TYPE MARK" );

     DECLARE

          TYPE PERSON (SEX : GENDER) IS
               RECORD
                    NULL;
               END RECORD;

          SUBTYPE WOMAN IS PERSON (IDENT (FEMALE));
          TOM : PERSON (MALE) := (SEX => IDENT (MALE));

     BEGIN
          IF WOMAN'(TOM) = PERSON'(SEX => MALE) THEN
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE WOMAN - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE WOMAN - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR OPERAND WITH " &
                        "DISC NOT EQUAL TO THOSE OF SUBTYPE WOMAN" );
     END;

     DECLARE
          TYPE PAIR (SEX1, SEX2 : GENDER) IS
               RECORD
                    NULL;
               END RECORD;

          SUBTYPE COUPLE IS PAIR (IDENT (FEMALE), IDENT (MALE));
          JONESES : PAIR (IDENT (MALE), IDENT (FEMALE));

     BEGIN
          IF COUPLE'(JONESES) = PAIR'(SEX1 => MALE, SEX2 => FEMALE)
             THEN
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE COUPLE - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE COUPLE - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR OPERAND WITH " &
                        "DISC NOT EQUAL TO THOSE OF SUBTYPE COUPLE" );
     END;

     DECLARE

          PACKAGE PKG IS
               TYPE PERSON (SEX : GENDER) IS PRIVATE;
               SUBTYPE MAN IS PERSON (IDENT (MALE));

               TESTWRITER : CONSTANT PERSON;

          PRIVATE
               TYPE PERSON (SEX : GENDER) IS
                    RECORD
                         NULL;
                    END RECORD;

               TESTWRITER : CONSTANT PERSON := (SEX => FEMALE);

          END PKG;

          USE PKG;

          ROSA : PERSON (IDENT (FEMALE));

     BEGIN
          IF MAN'(ROSA) = TESTWRITER THEN
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE MAN - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE MAN - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR OPERAND WITH " &
                        "DISC NOT EQUAL TO THOSE OF SUBTYPE MAN" );
     END;

     DECLARE
          PACKAGE PKG IS
               TYPE PAIR (SEX1, SEX2 : GENDER) IS PRIVATE;
               SUBTYPE FRIENDS IS PAIR (IDENT (FEMALE), IDENT (MALE));

               ALICE_AND_JERRY : CONSTANT FRIENDS;

          PRIVATE
               TYPE PAIR (SEX1, SEX2 : GENDER) IS
                    RECORD
                         NULL;
                    END RECORD;

               ALICE_AND_JERRY : CONSTANT FRIENDS :=
                                 (IDENT (FEMALE), IDENT (MALE));

          END PKG;

          USE PKG;

          DICK_AND_JOE : PAIR (IDENT (MALE), IDENT (MALE));

     BEGIN
          IF FRIENDS'(DICK_AND_JOE) = ALICE_AND_JERRY THEN
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE FRIENDS - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH DISC " &
                        "NOT EQUAL TO THOSE OF SUBTYPE FRIENDS - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR OPERAND WITH " &
                        "DISC NOT EQUAL TO THOSE OF SUBTYPE FRIENDS" );
     END;

     DECLARE

          PACKAGE PKG1 IS
               TYPE PERSON (SEX : GENDER) IS LIMITED PRIVATE;
               SUBTYPE ANDROID IS PERSON (IDENT (NEUTER));

               FUNCTION F RETURN PERSON;
               FUNCTION "=" (A, B : PERSON) RETURN BOOLEAN;
          PRIVATE
               TYPE PERSON (SEX : GENDER) IS
                    RECORD
                         NULL;
                    END RECORD;

          END PKG1;

          PACKAGE BODY PKG1 IS

               FUNCTION F RETURN PERSON IS
               BEGIN
                    RETURN PERSON'(SEX => (IDENT (MALE)));
               END F;

               FUNCTION "=" (A, B : PERSON) RETURN BOOLEAN IS
               BEGIN
                    RETURN A.SEX = B.SEX;
               END;

          END PKG1;

          PACKAGE PKG2 IS END PKG2;

          PACKAGE BODY PKG2 IS
               USE PKG1;

          BEGIN
               IF ANDROID'(F) = F THEN
                    FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH " &
                             "DISC NOT EQUAL TO THOSE OF SUBTYPE " &
                             "ANDROID - 1");
               ELSE
                    FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH " &
                             "DISC NOT EQUAL TO THOSE OF SUBTYPE " &
                             "ANDROID - 2");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR OPERAND " &
                             "WITH DISC NOT EQUAL TO THOSE OF " &
                             "SUBTYPE ANDROID" );
          END PKG2;

     BEGIN
          NULL;
     END;

     DECLARE
          PACKAGE PKG1 IS
               TYPE PAIR (SEX1, SEX2 : GENDER) IS LIMITED PRIVATE;
               SUBTYPE LOVERS IS PAIR (IDENT (FEMALE), IDENT (MALE));

               FUNCTION F RETURN PAIR;
               FUNCTION "=" (A, B : PAIR) RETURN BOOLEAN;
          PRIVATE
               TYPE PAIR (SEX1, SEX2 : GENDER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END PKG1;

          PACKAGE BODY PKG1 IS

               FUNCTION F RETURN PAIR IS
               BEGIN
                    RETURN PAIR'(SEX1 => (IDENT (FEMALE)),
                                   SEX2 => (IDENT (FEMALE)));
               END F;

               FUNCTION "=" (A, B : PAIR) RETURN BOOLEAN IS
               BEGIN
                    RETURN A.SEX1 = B.SEX2;
               END;

          END PKG1;

          PACKAGE PKG2 IS END PKG2;

          PACKAGE BODY PKG2 IS
               USE PKG1;

          BEGIN
               IF LOVERS'(F) = F THEN
                    FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH " &
                             "DISC NOT EQUAL TO THOSE OF SUBTYPE " &
                             "LOVERS - 1");
               ELSE
                    FAILED ( "NO EXCEPTION RAISED FOR OPERAND WITH " &
                             "DISC NOT EQUAL TO THOSE OF SUBTYPE " &
                             "LOVERS - 2");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR OPERAND " &
                             "WITH DISC NOT EQUAL TO THOSE OF " &
                             "SUBTYPE LOVERS" );
          END PKG2;

     BEGIN
          NULL;
     END;

     RESULT;
END C47008A;
