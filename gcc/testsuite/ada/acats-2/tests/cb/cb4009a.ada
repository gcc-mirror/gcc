-- CB4009A.ADA

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
-- CHECK THAT A PROGRAMMER DEFINED EXCEPTION AND A REDECLARED
-- PREDEFINED EXCEPTION MAY BE PROPAGATED OUT OF SCOPE AND BACK IN,
-- WITH OUT-OF-SCOPE 'OTHERS' HANDLERS HANDLING THE EXCEPTION
-- INSTEAD OF OTHER HANDLERS. SEPARATELY COMPILED UNITS ARE NOT TESTED.

-- DAT 4/15/81
-- SPS 1/14/82

WITH REPORT; USE REPORT;

PROCEDURE CB4009A IS

     E : EXCEPTION;

     I : INTEGER := 0;

     PROCEDURE P1 (C : INTEGER);
     PROCEDURE P2 (C : INTEGER);
     PROCEDURE P3 (C : INTEGER);

     F : BOOLEAN := FALSE;
     T : CONSTANT BOOLEAN := TRUE;

     PROCEDURE P1 (C : INTEGER) IS
     BEGIN
          P3(C);
     EXCEPTION
          WHEN E => F := T;
          WHEN CONSTRAINT_ERROR => F := T;
          WHEN OTHERS => I := I + 1; RAISE;
     END P1;

     PROCEDURE P2 (C : INTEGER) IS
          E : EXCEPTION;
          CONSTRAINT_ERROR : EXCEPTION;
     BEGIN
          CASE C IS
               WHEN 0 => FAILED ("WRONG CASE");
               WHEN 1 => RAISE E;
               WHEN -1 => RAISE CONSTRAINT_ERROR;
               WHEN OTHERS => P1 (C - C/ABS(C));
          END CASE;
     EXCEPTION
          WHEN E =>
               I := I + 100; RAISE;
          WHEN CONSTRAINT_ERROR =>
               I := I + 101; RAISE;
          WHEN OTHERS =>
               F := T;
     END P2;

     PROCEDURE P3 (C : INTEGER) IS
     BEGIN
          P2(C);
     EXCEPTION
          WHEN E => F := T;
          WHEN CONSTRAINT_ERROR => F := T;
     END P3;

BEGIN
     TEST ("CB4009A", "EXCEPTIONS PROPAGATED OUT OF SCOPE");

     I := 0;
     BEGIN
          P3 (-2);
          FAILED ("EXCEPTION NOT RAISED 1");
     EXCEPTION
          WHEN OTHERS => NULL;
     END;
     IF I /= 203 THEN
          FAILED ("INCORRECT HANDLER SOMEWHERE 1");
     END IF;

     I := 0;
     BEGIN
          P3(3);
          FAILED ("EXCEPTION NOT RAISED 2");
     EXCEPTION
          WHEN OTHERS => NULL;
     END;
     IF I /= 302 THEN
          FAILED ("INCORRECT HANDLER SOMEWHERE 2");
     END IF;

     IF F = T THEN
          FAILED ("WRONG HANDLER SOMEWHERE");
     END IF;

     RESULT;
END CB4009A;
