-- C42007E.ADA

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
-- CHECK THAT THE BOUNDS OF A STRING LITERAL ARE DETERMINED CORRECTLY.
-- IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY 'FIRST OF THE
-- INDEX SUBTYPE WHEN THE STRING LITERAL IS USED AS:

--   E) THE LEFT OR RIGHT OPERAND OF "&".

-- TBN  7/28/86

WITH REPORT; USE REPORT;
PROCEDURE C42007E IS

BEGIN

     TEST("C42007E", "CHECK THE BOUNDS OF A STRING LITERAL WHEN USED " &
                     "AS THE LEFT OR RIGHT OPERAND OF THE CATENATION " &
                     "OPERATOR");

     BEGIN

CASE_E :  DECLARE

               SUBTYPE STR_RANGE IS INTEGER RANGE 2 .. 10;
               TYPE STR IS ARRAY (STR_RANGE RANGE <>) OF CHARACTER;

               FUNCTION CONCAT1 RETURN STR IS
               BEGIN
                    RETURN ("ABC" & (7 .. 8 => 'D'));
               END CONCAT1;

               FUNCTION CONCAT2 RETURN STR IS
               BEGIN
                    RETURN ((IDENT_INT(4) .. 3 => 'A') & "BC");
               END CONCAT2;

               FUNCTION CONCAT3 RETURN STRING IS
               BEGIN
                    RETURN ("TEST" & (7 .. 8 => 'X'));
               END CONCAT3;

               FUNCTION CONCAT4 RETURN STRING IS
               BEGIN
                    RETURN ((8 .. 5 => 'A') & "DE");
               END CONCAT4;

          BEGIN

               IF CONCAT1'FIRST /= IDENT_INT(2) THEN
                    FAILED ("LOWER BOUND INCORRECTLY DETERMINED - 1");
               END IF;
               IF CONCAT1'LAST /= 6 THEN
                    FAILED ("UPPER BOUND INCORRECTLY DETERMINED - 1");
               END IF;
               IF CONCAT1 /= "ABCDD" THEN
                    FAILED ("STRING INCORRECTLY DETERMINED - 1");
               END IF;

               IF CONCAT2'FIRST /= IDENT_INT(2) THEN
                    FAILED ("LOWER BOUND INCORRECTLY DETERMINED - 2");
               END IF;
               IF CONCAT2'LAST /= 3 THEN
                    FAILED ("UPPER BOUND INCORRECTLY DETERMINED - 2");
               END IF;
               IF CONCAT2 /= "BC" THEN
                    FAILED ("STRING INCORRECTLY DETERMINED - 2");
               END IF;

               IF CONCAT3'FIRST /= IDENT_INT(1) THEN
                    FAILED ("LOWER BOUND INCORRECTLY DETERMINED - 3");
               END IF;
               IF CONCAT3'LAST /= 6 THEN
                    FAILED ("UPPER BOUND INCORRECTLY DETERMINED - 3");
               END IF;
               IF CONCAT3 /= "TESTXX" THEN
                    FAILED ("STRING INCORRECTLY DETERMINED - 3");
               END IF;

               IF CONCAT4'FIRST /= IDENT_INT(1) THEN
                    FAILED ("LOWER BOUND INCORRECTLY DETERMINED - 4");
               END IF;
               IF CONCAT4'LAST /= 2 THEN
                    FAILED ("UPPER BOUND INCORRECTLY DETERMINED - 4");
               END IF;
               IF CONCAT4 /= "DE" THEN
                    FAILED ("STRING INCORRECTLY DETERMINED - 4");
               END IF;

          END CASE_E;

     END;

     RESULT;

END C42007E;
