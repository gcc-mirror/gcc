-- C43214B.ADA

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
-- CHECK THAT THE LOWER BOUND FOR THE STRING LITERAL IS DETERMINED BY
-- THE APPLICABLE INDEX CONSTRAINT, WHEN ONE EXISTS.

-- EG  02/10/84

WITH REPORT;

PROCEDURE C43214B IS

     USE REPORT;

BEGIN

     TEST("C43214B", "SUBPROGRAM WITH CONSTRAINED ARRAY FORMAL " &
                     "PARAMETER");

     BEGIN

CASE_A :  BEGIN

--             COMMENT ("CASE A1 : SUBPROGRAM WITH CONSTRAINED " &
--                      "ONE-DIMENSIONAL ARRAY FORMAL PARAMETER");

     CASE_A1 : DECLARE

                    SUBTYPE STA1 IS STRING(IDENT_INT(11) .. 15);

                    PROCEDURE PROC1 (A : STA1) IS
                    BEGIN
                         IF A'FIRST /= 11 THEN
                              FAILED ("CASE 1 : LOWER BOUND " &
                                      "INCORRECT");
                         ELSIF A'LAST /= 15 THEN
                              FAILED ("CASE 1 : UPPER BOUND " &
                                      "INCORRECT");
                         ELSIF A /= "ABCDE" THEN
                              FAILED ("CASE 1 : ARRAY DOES NOT " &
                                     "CONTAIN THE CORRECT VALUES");
                         END IF;
                    END;

               BEGIN

                    PROC1 ("ABCDE");

               END CASE_A1;

--             COMMENT ("CASE A2 : SUBPROGRAM WITH CONSTRAINED " &
--                      "TWO-DIMENSIONAL ARRAY FORMAL PARAMETER");

     CASE_A2 : DECLARE

                    TYPE TA IS ARRAY (11 .. 12, 10 .. 11) OF CHARACTER;

                    PROCEDURE PROC1 (A : TA) IS
                    BEGIN
                         IF A'FIRST(1) /= 11 OR A'FIRST(2) /= 10 THEN
                              FAILED ("CASE 2 : LOWER BOUND " &
                                      "INCORRECT");
                         ELSIF A'LAST(1) /= 12 OR A'LAST(2) /= 11 THEN
                              FAILED ("CASE 2 : UPPER BOUND " &
                                      "INCORRECT");
                         ELSIF A /= ("AB", "CD") THEN
                              FAILED ("CASE 2 : ARRAY DOES NOT " &
                                      "CONTAIN THE CORRECT VALUES");
                         END IF;
                    END;

               BEGIN

                    PROC1 (("AB", "CD"));

               END CASE_A2;

          END CASE_A;

     END;

     RESULT;

END C43214B;
