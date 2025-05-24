-- C54A13C.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A QUALIFIED EXPRESSION, A
--     TYPE CONVERSION, OR ONE OF THESE IN PARENTHESES, AND ITS
--     SUBTYPE IS NONSTATIC, THEN ANY VALUE OF THE EXPRESSION'S
--     BASE TYPE MAY APPEAR AS A CHOICE.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C54A13C IS

     L : INTEGER := 1;
     R : INTEGER := 100;

     SUBTYPE INT IS INTEGER RANGE L .. R;

     A : INT := 50;

     B : INTEGER := 50;

     C : INTEGER;

BEGIN
     TEST ("C54A13C", "CHECK THAT IF A CASE EXPRESSION IS A " &
                      "QUALIFIED EXPRESSION, A TYPE CONVERSION, " &
                      "OR ONE OF THESE IN PARENTHESES, AND ITS " &
                      "SUBTYPE IS NONSTATIC, THEN ANY VALUE OF THE " &
                      "EXPRESSION'S BASE TYPE MAY APPEAR AS A CHOICE");

     CASE INT'(A) IS
          WHEN 0 => C := IDENT_INT (5);
          WHEN 50 => C := IDENT_INT (10);
          WHEN -3000 => C := IDENT_INT (15);
          WHEN OTHERS => C := IDENT_INT (20);
     END CASE;

     IF C /= IDENT_INT (10) THEN
          FAILED ("INCORRECT CHOICE MADE FOR QUALIFIED EXPRESSION IN " &
                  "CASE");
     END IF;

     CASE INT(B) IS
          WHEN 0 => C := IDENT_INT (5);
          WHEN 50 => C := IDENT_INT (10);
          WHEN -3000 => C := IDENT_INT (15);
          WHEN OTHERS => C := IDENT_INT (20);
     END CASE;

     IF C /= IDENT_INT (10) THEN
          FAILED ("INCORRECT CHOICE MADE FOR TYPE CONVERSION IN CASE");
     END IF;

     CASE (INT'(A)) IS
          WHEN 0 => C := IDENT_INT (5);
          WHEN 50 => C := IDENT_INT (10);
          WHEN -3000 => C := IDENT_INT (15);
          WHEN OTHERS => C := IDENT_INT (20);
     END CASE;

     IF C /= IDENT_INT (10) THEN
          FAILED ("INCORRECT CHOICE MADE FOR QUALIFIED EXPRESSION IN " &
                  "PARENTHESES IN CASE");
     END IF;

     CASE (INT(B)) IS
          WHEN 0 => C := IDENT_INT (5);
          WHEN 50 => C := IDENT_INT (10);
          WHEN -3000 => C := IDENT_INT (15);
          WHEN OTHERS => C := IDENT_INT (20);
     END CASE;

     IF C /= IDENT_INT (10) THEN
          FAILED ("INCORRECT CHOICE MADE FOR TYPE CONVERSION IN " &
                  "PARENTHESES IN CASE");
     END IF;

     RESULT;
END C54A13C;
