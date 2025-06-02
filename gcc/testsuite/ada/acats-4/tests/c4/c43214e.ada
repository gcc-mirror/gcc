-- C43214E.ADA

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

PROCEDURE C43214E IS

     USE REPORT;

BEGIN

     TEST("C43214E", "INITIALIZATION OF CONSTRAINED ARRAY");

     BEGIN

CASE_D :  BEGIN

--             COMMENT ("CASE D1 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY CONSTANT");

     CASE_D1 : DECLARE

                    D1 : CONSTANT STRING(11 .. 13) := "ABC";

               BEGIN

                    IF D1'FIRST /= 11 THEN
                         FAILED ("CASE 1 : LOWER BOUND INCORRECT");
                    ELSIF D1'LAST /= 13 THEN
                         FAILED ("CASE 1 : UPPER BOUND INCORRECT");
                    ELSIF D1 /= "ABC" THEN
                              FAILED ("CASE 1 : ARRAY DOES NOT " &
                                      "CONTAIN THE CORRECT VALUES");
                    END IF;

               END CASE_D1;

--             COMMENT ("CASE D2 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY VARIABLE");

     CASE_D2 : DECLARE

                    D2 : STRING(11 .. 13) := "ABC";

               BEGIN

                    IF D2'FIRST /= 11 THEN
                         FAILED ("CASE 2 : LOWER BOUND INCORRECT");
                    ELSIF D2'LAST /= 13 THEN
                         FAILED ("CASE 2 : UPPER BOUND INCORRECT");
                    ELSIF D2 /= "ABC" THEN
                         FAILED ("CASE 2 : INCORRECT VALUES");
                    END IF;

               END CASE_D2;

--             COMMENT ("CASE D3 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY FORMAL PARAMETER OF A SUBPROGRAM");

     CASE_D3 : DECLARE

                    SUBTYPE STD3 IS STRING(IDENT_INT(5) .. 7);

                    PROCEDURE PROC1 (A : STD3 := "ABC") IS
                    BEGIN
                         IF A'FIRST /= 5 THEN
                              FAILED ("CASE 3 : LOWER BOUND " &
                                      "INCORRECT");
                         ELSIF A'LAST /= 7 THEN
                              FAILED ("CASE 3 : UPPER BOUND " &
                                      "INCORRECT");
                         ELSIF A /= "ABC" THEN
                              FAILED ("CASE 3 : INCORRECT VALUES");
                         END IF;
                    END PROC1;

               BEGIN

               PROC1;

               END CASE_D3;

--             COMMENT ("CASE D4 : INITIALIZATION OF CONSTRAINED " &
--                      "ARRAY FORMAL PARAMETER OF A GENERIC UNIT");

     CASE_D4 : DECLARE

                    SUBTYPE STD4 IS STRING(5 .. 8);

                    GENERIC
                         D4 : STD4 := "ABCD";
                    PROCEDURE PROC1;

                    PROCEDURE PROC1 IS
                    BEGIN
                         IF D4'FIRST /= 5 THEN
                              FAILED ("CASE 4 : LOWER BOUND " &
                                      "INCORRECT");
                         ELSIF D4'LAST /= 8 THEN
                              FAILED ("CASE 4 : UPPER BOUND " &
                                      "INCORRECT");
                         ELSIF D4 /= "ABCD" THEN
                              FAILED ("CASE 4 : INCORRECT VALUES");
                         END IF;
                    END PROC1;

                    PROCEDURE PROC2 IS NEW PROC1;

               BEGIN

                    PROC2;

               END CASE_D4;

          END CASE_D;

     END;

     RESULT;

END C43214E;
