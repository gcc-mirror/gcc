-- C54A13B.ADA

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
--     CHECK THAT IF A CASE EXPRESSION IS A GENERIC "IN" OR "IN OUT"
--     PARAMETER WITH A NON-STATIC SUBTYPE OR ONE OF THESE IN
--     PARENTHESES, THEN ANY VALUE OF THE EXPRESSION'S BASE TYPE MAY
--     APPEAR AS A CHOICE.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C54A13B IS

     L : INTEGER := IDENT_INT(1);
     R : INTEGER := IDENT_INT(100);

     SUBTYPE INT IS INTEGER RANGE L .. R;

     GENERIC
          IN_PAR : IN INT;
          IN_OUT_PAR : IN OUT INT;
     PROCEDURE GEN_PROC (I : IN OUT INTEGER);

     IN_VAR : INT := IDENT_INT (10);
     IN_OUT_VAR : INT := IDENT_INT (100);
     CHECK_VAR : INT := IDENT_INT (1);

     PROCEDURE GEN_PROC (I : IN OUT INTEGER) IS
     BEGIN
          CASE IN_PAR IS
               WHEN 0 => I := I + IDENT_INT (2);
               WHEN 10 => I := I + IDENT_INT (1);
               WHEN -3000 => I := I + IDENT_INT (3);
               WHEN OTHERS => I := I + IDENT_INT (4);
          END CASE;

          CASE IN_OUT_PAR IS
               WHEN 0 => IN_OUT_PAR := IDENT_INT (0);
               WHEN 100 => IN_OUT_PAR := IDENT_INT (50);
               WHEN -3000 => IN_OUT_PAR := IDENT_INT (-3000);
               WHEN OTHERS => IN_OUT_PAR := IDENT_INT (5);
          END CASE;

          CASE (IN_PAR) IS
               WHEN 0 => I := I + IDENT_INT (2);
               WHEN 10 => I := I + IDENT_INT (1);
               WHEN -3000 => I := I + IDENT_INT (3);
               WHEN OTHERS => I := I + IDENT_INT (4);
          END CASE;

          CASE (IN_OUT_PAR) IS
               WHEN 0 => IN_OUT_PAR := IDENT_INT (200);
               WHEN 50 => IN_OUT_PAR := IDENT_INT (25);
               WHEN -3000 => IN_OUT_PAR := IDENT_INT (300);
               WHEN OTHERS => IN_OUT_PAR := IDENT_INT (400);
          END CASE;

     END GEN_PROC;

     PROCEDURE P IS NEW GEN_PROC (IN_VAR, IN_OUT_VAR);

BEGIN
     TEST ("C54A13B", "CHECK THAT IF A CASE EXPRESSION IS A " &
                      "GENERIC 'IN' OR 'IN OUT' PARAMETER WITH A " &
                      "NON-STATIC SUBTYPE OR ONE OF " &
                      "THESE IN PARENTHESES, THEN ANY VALUE OF " &
                      "THE EXPRESSION'S BASE TYPE MAY APPEAR AS " &
                      "A CHOICE");

     P (CHECK_VAR);

     IF NOT EQUAL (CHECK_VAR, IDENT_INT(3)) THEN
          FAILED ("INCORRECT CHOICES MADE FOR IN PARAMETER IN CASE");
     END IF;

     IF NOT EQUAL (IN_OUT_VAR, IDENT_INT(25)) THEN
          FAILED ("INCORRECT CHOICESMADE FOR IN OUT PARAMETER IN CASE");
     END IF;

     RESULT;
END C54A13B;
