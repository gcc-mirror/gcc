-- A2A031A.ADA

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
-- CHECK THAT AN EXCLAMATION MARK CAN REPLACE A VERTICAL BAR WHEN THE
-- VERTICAL BAR IS USED AS A SEPARATOR.

-- CONTEXTS ARE:
--   AS A CHOICE IN A VARIANT PART
--   IN A DISCRIMINANT CONSTRAINT
--   IN A CASE STATEMENT CHOICE
--   IN AN AGGREGATE
--   IN AN EXCEPTION HANDLER.

-- JBG 5/25/85

WITH REPORT; USE REPORT;
PROCEDURE A2A031A IS

     TYPE ENUM IS (E1, E2, E3);
     TYPE REC (A, B : ENUM) IS
          RECORD
               C : INTEGER;
               CASE A IS
                    WHEN E1 ! E2 =>     -- CHOICE OF VARIANT.
                         D : INTEGER;
                    WHEN E3 =>
                         E : FLOAT;
               END CASE;
          END RECORD;

     EX1, EX2, EX3 : EXCEPTION;

     VAR  : REC (A!B => E2);            -- DISCRIMINANT CONSTRAINT.

     EVAR : ENUM := E2;

BEGIN

     TEST ("A2A031A", "CHECK USE OF ! AS SEPARATOR IN PLACE OF |");

     CASE EVAR IS
          WHEN E3 => NULL;
          WHEN E2!E1 => NULL;           -- CASE STATEMENT CHOICE.
     END CASE;

     VAR := (A!B => E2, C ! D => 0);    -- AGGREGATE.

     RESULT;
EXCEPTION
     WHEN EX1!EX2 ! EX3 => NULL;        -- EXCEPTION HANDLER.
END A2A031A;
