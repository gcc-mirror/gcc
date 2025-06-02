-- C84009A.ADA

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
--     CHECK THAT A USE CLAUSE MAKES AN IMPLICITLY OR EXPLICITLY
--     DECLARED OPERATOR DIRECTLY VISIBLE IF NO HOMOGRAPH OF THE
--     OPERATOR IS ALREADY DIRECTLY VISIBLE.

-- HISTORY:
--     JET 03/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C84009A IS

     TYPE INT IS NEW INTEGER RANGE -100 .. 100;

     PACKAGE PACK IS
          FUNCTION "+" (LEFT : INTEGER; RIGHT : INT) RETURN INTEGER;
          FUNCTION "-" (LEFT, RIGHT : INT) RETURN INT;
          FUNCTION "-" (RIGHT : INT) RETURN INTEGER;
          FUNCTION "+" (RIGHT : INT) RETURN INTEGER;
     END PACK;

     FUNCTION "+" (RIGHT : INT) RETURN INTEGER IS
     BEGIN
          RETURN INTEGER'(1) + INTEGER(RIGHT);
     END "+";

     PACKAGE BODY PACK IS
          FUNCTION "+" (LEFT : INTEGER; RIGHT : INT) RETURN INTEGER IS
          BEGIN
               RETURN LEFT + INTEGER(RIGHT);
          END "+";

          FUNCTION "-" (LEFT, RIGHT : INT) RETURN INT IS
          BEGIN
               FAILED ("BINARY ""-"" ALREADY VISIBLE FOR TYPE INT");
               RETURN LEFT + (-RIGHT);
          END "-";

          FUNCTION "-" (RIGHT : INT) RETURN INTEGER IS
          BEGIN
               RETURN INTEGER'(0) - INTEGER(RIGHT);
          END "-";

          FUNCTION "+" (RIGHT : INT) RETURN INTEGER IS
          BEGIN
               FAILED ("UNARY ""+"" ALREADY VISIBLE FOR TYPE INT");
               RETURN INTEGER'(0) + INTEGER(RIGHT);
          END "+";
     END PACK;

     USE PACK;

BEGIN
     TEST ("C84009A", "CHECK THAT A USE CLAUSE MAKES AN IMPLICITLY " &
                      "OR EXPLICITLY DECLARED OPERATOR DIRECTLY " &
                      "VISIBLE IF NO HOMOGRAPH OF THE OPERATOR IS " &
                      "ALREADY DIRECTLY VISIBLE");

     IF INTEGER'(10) + INT'(10) /= IDENT_INT(20) THEN
          FAILED ("INCORRECT RESULT FROM BINARY ""+""");
     END IF;

     IF INT'(5) - INT'(3) /= INT'(2) THEN
          FAILED ("INCORRECT RESULT FROM BINARY ""-""");
     END IF;

     IF -INT'(20) /= IDENT_INT(-INTEGER'(20)) THEN
          FAILED ("INCORRECT RESULT FROM UNARY ""-""");
     END IF;

     IF +INT'(20) /= IDENT_INT(+INTEGER'(21)) THEN
          FAILED ("INCORRECT RESULT FROM UNARY ""+""");
     END IF;

     RESULT;
END C84009A;
