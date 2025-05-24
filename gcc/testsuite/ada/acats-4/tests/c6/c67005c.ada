-- C67005C.ADA

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
-- CHECK THAT A DECLARATION OF "=" NEED NOT HAVE PARAMETERS
-- OF A LIMITED TYPE IN A RENAMING DECLARATION. THIS TEST CHECKS
-- ACCESS TYPES.

-- BRYCE BARDIN (HUGHES AIRCRAFT) 7/2/84
-- CPP 7/12/84

WITH REPORT;  USE REPORT;
PROCEDURE C67005C IS

     GENERIC
          TYPE T IS LIMITED PRIVATE;
          WITH FUNCTION EQUAL (LEFT, RIGHT : T) RETURN BOOLEAN IS <>;
     PACKAGE EQUALITY IS
          FUNCTION "=" (LEFT, RIGHT : T) RETURN BOOLEAN;
          -- PRAGMA INLINE ("=");
     END EQUALITY;

     PACKAGE BODY EQUALITY IS
          FUNCTION "=" (LEFT, RIGHT : T) RETURN BOOLEAN IS
          BEGIN
               RETURN EQUAL (LEFT, RIGHT);
          END "=";
     END EQUALITY;

     PACKAGE STARTER IS
          TYPE INT IS PRIVATE;
          FUNCTION VALUE_OF (I : INTEGER) RETURN INT;
          FUNCTION EQUAL (LEFT, RIGHT : INT) RETURN BOOLEAN;
     PRIVATE
          TYPE INT IS ACCESS INTEGER;
     END STARTER;

     PACKAGE BODY STARTER IS
          FUNCTION VALUE_OF (I : INTEGER) RETURN INT IS
          BEGIN
               RETURN NEW INTEGER'(I);
          END VALUE_OF;

          FUNCTION EQUAL (LEFT, RIGHT : INT) RETURN BOOLEAN IS
          BEGIN
               RETURN LEFT.ALL = RIGHT.ALL;
          END EQUAL;
     END STARTER;

     PACKAGE ABSTRACTION IS
          TYPE INT IS NEW STARTER.INT;
          PACKAGE INT_EQUALITY IS NEW EQUALITY (INT, EQUAL);
          FUNCTION "=" (LEFT, RIGHT : INT) RETURN BOOLEAN
               RENAMES INT_EQUALITY."=";
     END ABSTRACTION;    
     USE ABSTRACTION;

BEGIN

     TEST ("C67005C", "RENAMING OF EQUALITY OPERATOR WITH " &
           "NON-LIMITED PARAMETERS");

     DECLARE

          I : INT := VALUE_OF(1);
          J : INT := VALUE_OF(0);

          PROCEDURE CHECK (B : BOOLEAN) IS
          BEGIN
               IF I = J AND B THEN
                    COMMENT ("I = J");
               ELSIF I /= J AND NOT B THEN
                    COMMENT ("I /= J");
               ELSE
                    FAILED ("WRONG ""="" OPERATOR");
               END IF;
          END CHECK;

     BEGIN

          CHECK(FALSE);
          I := VALUE_OF(0);
          CHECK(TRUE);
     
          RESULT;

     END;

END C67005C;
