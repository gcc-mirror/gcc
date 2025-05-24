-- C43214D.ADA

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

PROCEDURE C43214D IS

     USE REPORT;

BEGIN

     TEST("C43214D", "CONSTRAINED FUNCTION RESULT TYPE");

     BEGIN

CASE_C :  DECLARE

               TYPE TC IS ARRAY (INTEGER RANGE -1 .. 0, 
                                 IDENT_INT(7) .. 9) OF CHARACTER;

               FUNCTION FUN1 (A : INTEGER) RETURN TC IS
               BEGIN
                    RETURN ("ABC", "DEF");
               END;

          BEGIN

               IF FUN1(5)'FIRST(1) /= -1 THEN
                    FAILED ("LOWER BOUND INCORRECT " &
                            "FOR 'FIRST(1)");
               ELSIF FUN1(5)'FIRST(2) /= 7 THEN
                    FAILED ("LOWER BOUND INCORRECT " &
                            "FOR 'FIRST(2)");
               ELSIF FUN1(5)'LAST(1) /= 0 THEN
                    FAILED ("UPPER BOUND INCORRECT " &
                            "FOR 'LAST(1)");
               ELSIF FUN1(5)'LAST(2) /= 9 THEN
                    FAILED ("UPPER BOUND INCORRECT " &
                            "FOR 'LAST(2)");
               ELSIF FUN1(5) /= ("ABC", "DEF") THEN
                    FAILED ("FUNCTION DOES NOT " &
                            "RETURN THE CORRECT VALUES");
               END IF;

          END CASE_C;

     END;

     RESULT;

END C43214D;
