-- C54A13D.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A FUNCTION INVOCATION,
--     ATTRIBUTE, STATIC EXPRESSION, OR ONE OF THESE IN PARENTHESES,
--     THEN ANY VALUE OF THE EXPRESSION'S BASE TYPE MAY APPEAR AS A
--     CHOICE.

-- HISTORY:
--     BCB 07/19/88  CREATED ORIGINAL TEST.
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     GJD 11/15/95  REMOVED ADA 95 INCOMPATIBLE ALTERNATIVE IN FIRST CASE.

WITH REPORT; USE REPORT;

PROCEDURE C54A13D IS

     SUBTYPE INT IS INTEGER RANGE -100 .. 100;

     CONS : CONSTANT INT := 0;

     C : INT;

     TYPE ENUM IS (ONE, TWO, THREE, FOUR, FIVE, SIX);

     SUBTYPE SUBENUM IS ENUM RANGE THREE .. FOUR;

     FUNCTION FUNC RETURN INT IS
     BEGIN
          RETURN 0;
     END FUNC;

BEGIN
     TEST ("C54A13D", "CHECK THAT IF A CASE EXPRESSION IS A FUNCTION " &
                      "INVOCATION, ATTRIBUTE, STATIC EXPRESSION, OR " &
                      "ONE OF THESE IN PARENTHESES, THEN ANY VALUE " &
                      "OF THE EXPRESSION'S BASE TYPE MAY APPEAR AS " &
                      "A CHOICE");

     CASE FUNC IS
          WHEN 0 => C := IDENT_INT (5);
          WHEN 100 => C := IDENT_INT (10);
          WHEN OTHERS => C := IDENT_INT (20);
     END CASE;

     IF NOT EQUAL (C,5) THEN
          FAILED ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
                  "FUNCTION INVOCATION - 1");
     END IF;

     CASE (FUNC) IS
          WHEN 0 => C := IDENT_INT (25);
          WHEN 100 => C := IDENT_INT (50);
          WHEN -3000 => C := IDENT_INT (75);
          WHEN OTHERS => C := IDENT_INT (90);
     END CASE;

     IF NOT EQUAL (C,25) THEN
          FAILED ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
                  "FUNCTION INVOCATION - 2");
     END IF;

     CASE SUBENUM'FIRST IS
          WHEN ONE => C := IDENT_INT (100);
          WHEN TWO => C := IDENT_INT (99);
          WHEN THREE => C := IDENT_INT (98);
          WHEN FOUR => C := IDENT_INT (97);
          WHEN FIVE => C := IDENT_INT (96);
          WHEN SIX => C := IDENT_INT (95);
     END CASE;

     IF NOT EQUAL (C,98) THEN
          FAILED ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS AN " &
                  "ATTRIBUTE - 1");
     END IF;

     CASE (SUBENUM'FIRST) IS
          WHEN ONE => C := IDENT_INT (90);
          WHEN TWO => C := IDENT_INT (89);
          WHEN THREE => C := IDENT_INT (88);
          WHEN FOUR => C := IDENT_INT (87);
          WHEN FIVE => C := IDENT_INT (86);
          WHEN SIX => C := IDENT_INT (85);
     END CASE;

     IF NOT EQUAL (C,88) THEN
          FAILED ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS AN " &
                  "ATTRIBUTE - 2");
     END IF;

     CASE CONS * 1 IS
          WHEN 0 => C := IDENT_INT (1);
          WHEN 100 => C := IDENT_INT (2);
          WHEN -3000 => C := IDENT_INT (3);
          WHEN OTHERS => C := IDENT_INT (4);
     END CASE;

     IF NOT EQUAL (C,1) THEN
          FAILED ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
                  "STATIC EXPRESSION - 1");
     END IF;

     CASE (CONS * 1) IS
          WHEN 0 => C := IDENT_INT (10);
          WHEN 100 => C := IDENT_INT (20);
          WHEN -3000 => C := IDENT_INT (30);
          WHEN OTHERS => C := IDENT_INT (40);
     END CASE;

     IF NOT EQUAL (C,10) THEN
          FAILED ("IMPROPER CHOICE FOR CASE EXPRESSION WHICH IS A " &
                  "STATIC EXPRESSION - 2");
     END IF;

     RESULT;
END C54A13D;
