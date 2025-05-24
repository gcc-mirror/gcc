-- C43205C.ADA

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

--   C) THE RETURN EXPRESSION IN A FUNCTION WHOSE RETURN TYPE IS
--      UNCONSTRAINED.

-- EG  01/26/84

WITH REPORT;

PROCEDURE C43205C IS

     USE REPORT;

BEGIN

     TEST("C43205C", "CASE C : UNCONSTRAINED FUNCTION RESULT TYPE");

     BEGIN

CASE_C :  DECLARE

               SUBTYPE STC1 IS INTEGER RANGE -2 .. 3;
               SUBTYPE STC2 IS INTEGER RANGE 7 .. 20;
               TYPE TC IS ARRAY (STC1 RANGE <>, STC2 RANGE <>)
                                                        OF INTEGER;

               FUNCTION FUN1 (A : INTEGER) RETURN TC IS
               BEGIN
                    RETURN ((5, 4, 3), (2, IDENT_INT(1), 0));
               END;

          BEGIN

               IF FUN1(5)'FIRST(1) /= -2 THEN
                    FAILED ("CASE C : LOWER BOUND INCORRECTLY " &
                            "GIVEN BY 'FIRST(1)");
               ELSIF FUN1(5)'FIRST(2) /= 7 THEN
                    FAILED ("CASE C : LOWER BOUND INCORRECTLY " &
                            "GIVEN BY 'FIRST(2)");
               ELSIF FUN1(5)'LAST(1) /= -1 THEN
                    FAILED ("CASE C : UPPER BOUND INCORRECTLY " &
                            "GIVEN BY 'LAST(1)");
               ELSIF FUN1(5)'LAST(2) /= 9 THEN
                    FAILED ("CASE C : UPPER BOUND INCORRECTLY " &
                            "GIVEN BY 'LAST(2)");
               ELSIF FUN1(5) /= ((5, 4, 3), (2, 1, 0)) THEN
                    FAILED ("CASE C : FUNCTION DOES NOT " &
                            "RETURN THE CORRECT VALUES");
               END IF;

          END CASE_C;

     END;

     RESULT;

END C43205C;
