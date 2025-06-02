-- C43205E.ADA

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
-- CHECK THAT THE BOUNDS OF A POSITIONAL AGGREGATE ARE DETERMINED
-- CORRECTLY. IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY
-- 'FIRST OF THE INDEX SUBTYPE WHEN THE POSITIONAL AGGREGATE IS USED AS:

--   E) THE LEFT OR RIGHT OPERAND OF "&".

-- EG  01/26/84

WITH REPORT;

PROCEDURE C43205E IS

     USE REPORT;

BEGIN

     TEST("C43205E", "CASE E : OPERAND OF &");

     BEGIN

CASE_E :  DECLARE

               SUBTYPE STE IS INTEGER RANGE 2 .. 10;

               TYPE COLOR IS (RED, GREEN, BLUE);
               TYPE TE IS ARRAY (STE RANGE <>) OF COLOR;

               FUNCTION CONCAT1 RETURN TE IS
               BEGIN
                    RETURN (RED, GREEN, BLUE) & (7 .. 8 => RED);
               END;

               FUNCTION CONCAT2 RETURN TE IS
               BEGIN
                    RETURN (IDENT_INT(4) .. 3 => RED) & (GREEN, BLUE);
               END;

               FUNCTION CONCAT3 RETURN STRING IS
               BEGIN
                    RETURN "TEST" & (7 .. 8 => 'X');
               END;

               FUNCTION CONCAT4 RETURN STRING IS
               BEGIN
                    RETURN (8 .. 5 => 'A') & "BC";
               END;

          BEGIN

               IF CONCAT1'FIRST /= IDENT_INT(2) THEN
                    FAILED ("CASE E1 : LOWER BOUND INCORRECTLY " &
                            "GIVEN BY 'FIRST");
               ELSIF CONCAT1'LAST /= 6 THEN
                    FAILED ("CASE E1 : UPPER BOUND INCORRECTLY " &
                            "GIVEN BY 'LAST");
               ELSIF CONCAT1 /= (RED, GREEN, BLUE, RED, RED) THEN
                    FAILED ("CASE E1 : INCORRECT VALUES PRODUCED");
               END IF;
               IF CONCAT2'FIRST /= IDENT_INT(2) THEN
                    FAILED ("CASE E2 : LOWER BOUND INCORRECTLY " &
                            "GIVEN BY 'FIRST");
               ELSIF CONCAT2'LAST /= 3 THEN
                    FAILED ("CASE E2 : UPPER BOUND INCORRECTLY " &
                            "GIVEN BY 'LAST");
               ELSIF CONCAT2 /= (GREEN, BLUE) THEN
                    FAILED ("CASE E2 : INCORRECT VALUES PRODUCED");
               END IF;
               IF CONCAT3'FIRST /= IDENT_INT(1) THEN
                    FAILED ("CASE E3 : LOWER BOUND INCORRECTLY " &
                            "GIVEN BY 'FIRST");
               ELSIF CONCAT3'LAST /= 6 THEN
                    FAILED ("CASE E3 : UPPER BOUND INCORRECTLY " &
                            "GIVEN BY 'LAST");
               ELSIF CONCAT3 /= "TESTXX" THEN
                    FAILED ("CASE E3 : INCORRECT VALUES PRODUCED");
               END IF;
               IF CONCAT4'FIRST /= IDENT_INT(1) THEN
                    FAILED ("CASE E4 : LOWER BOUND INCORRECTLY " &
                            "GIVEN BY 'FIRST");
               ELSIF CONCAT4'LAST /= 2 THEN
                    FAILED ("CASE E4 : UPPER BOUND INCORRECTLY " &
                            "GIVEN BY 'LAST");
               ELSIF CONCAT4 /= "BC" THEN
                    FAILED ("CASE E4 : INCORRECT VALUES PRODUCED");
               END IF;

          END CASE_E;

     END;

     RESULT;

END C43205E;
