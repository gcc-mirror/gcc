-- CC3125C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A GENERIC IN PARAMETER
-- HAVING A FLOATING POINT TYPE IF AND ONLY IF THE VALUE OF THE ACTUAL
-- PARAMETER LIES OUTSIDE THE RANGE OF THE FORMAL PARAMETER.

-- TBN  12/15/86

WITH REPORT; USE REPORT;
PROCEDURE CC3125C IS

     TYPE FLT IS DIGITS 5 RANGE -10.0 .. 10.0;
     SUBTYPE FLO IS FLT RANGE -5.0 .. 5.0;

     FUNCTION IDENT_FLT (X : FLT) RETURN FLT IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN X;
          END IF;
          RETURN 0.0;
     END IDENT_FLT;

BEGIN
     TEST ("CC3125C", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
                      "GENERIC IN PARAMETER HAVING A FLOATING POINT " &
                      "TYPE IF AND ONLY IF THE VALUE OF THE ACTUAL " &
                      "PARAMETER LIES OUTSIDE THE RANGE OF THE " &
                      "FORMAL PARAMETER");
     DECLARE
          GENERIC
               GEN_FLO : IN FLO;
          PACKAGE P IS
               PAC_FLO : FLT := GEN_FLO;
          END P;
     BEGIN
          BEGIN
               DECLARE
                    PACKAGE P1 IS NEW P(IDENT_FLT(-5.0));
               BEGIN
                    IF P1.PAC_FLO /= IDENT_FLT(-5.0) THEN
                         FAILED ("INCORRECT VALUE PASSED - 1");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - 1");
          END;

          BEGIN
               DECLARE
                    PACKAGE P2 IS NEW P(IDENT_FLT(-5.1));
               BEGIN
                    FAILED ("NO EXCEPTION RAISED - 2");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
          END;

          BEGIN
               DECLARE
                    PACKAGE P3 IS NEW P(IDENT_FLT(5.1));
               BEGIN
                    FAILED ("NO EXCEPTION RAISED - 3");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 3");
          END;
     END;
     -------------------------------------------------------------------

     DECLARE
          GENERIC
               TYPE GEN_TYP IS DIGITS <>;
               GEN_FLO : IN GEN_TYP;
          PACKAGE Q IS
               PAC_FLO : GEN_TYP := GEN_FLO;
          END Q;
     BEGIN
          BEGIN
               DECLARE
                    PACKAGE Q1 IS NEW Q(FLO, IDENT_FLT(5.0));
               BEGIN
                    IF Q1.PAC_FLO /= IDENT_FLT(5.0) THEN
                         FAILED ("INCORRECT VALUE PASSED - 4");
                    END IF;
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - 4");
          END;

          BEGIN
               DECLARE
                    PACKAGE Q2 IS NEW Q(FLO, IDENT_FLT(-5.1));
               BEGIN
                    FAILED ("NO EXCEPTION RAISED - 5");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 5");
          END;

          BEGIN
               DECLARE
                    PACKAGE Q3 IS NEW Q(FLO, IDENT_FLT(5.1));
               BEGIN
                    FAILED ("NO EXCEPTION RAISED - 6");
               END;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 6");
          END;
     END;

     RESULT;
END CC3125C;
