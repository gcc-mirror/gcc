-- CB4002A.ADA

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
-- CHECK THAT EXCEPTIONS RAISED DURING ELABORATION OF THE
-- DECLARATIVE PART OF A SUBPROGRAM ARE PROPAGATED TO THE
-- CALLER, FOR CONSTRAINT_ERROR CAUSED BY INITIALIZATION,
-- AND CONSTRAINT ELABORATION, AND FOR FUNCTION EVALUATIONS
-- RAISING  CONSTRAINT_ERROR AND A PROGRAMMER-DEFINED EXCEPTION.

-- DAT 4/13/81
-- SPS 3/28/83

WITH REPORT; USE REPORT;

PROCEDURE CB4002A IS
BEGIN
     TEST("CB4002A", "EXCEPTIONS IN SUBPROGRAM DECLARATIVE_PARTS"
          & " ARE PROPAGATED TO CALLER");

     DECLARE
          SUBTYPE I5 IS INTEGER RANGE -5 .. 5;
     
          E : EXCEPTION;
     
          FUNCTION RAISE_IT (I : I5) RETURN INTEGER IS
               J : INTEGER RANGE 0 .. 1 := I;
          BEGIN
               IF I = 0 THEN
                    RAISE CONSTRAINT_ERROR;
               ELSIF I = 1 THEN
                    RAISE E;
               END IF;
               FAILED ("EXCEPTION NOT RAISED 0");
               RETURN J;
          EXCEPTION
               WHEN OTHERS =>
                    IF I NOT IN 0 .. 1 THEN
                         FAILED ("WRONG HANDLER 0");
                         RETURN 0;
                    ELSE
                         RAISE;
                    END IF;
          END RAISE_IT;
     
          PROCEDURE P1 (P : INTEGER) IS
               Q : INTEGER := RAISE_IT (P);
          BEGIN
               FAILED ("EXCEPTION NOT RAISED 1");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("WRONG HANDLER 1");
          END P1;

          PROCEDURE P2 (P : INTEGER) IS
               Q : I5 RANGE 0 .. P := 1;
          BEGIN
               IF P = 0 OR P > 5 THEN
                    FAILED ("EXCEPTION NOT RAISED 2");
               END IF;
          END P2;
     
     BEGIN
     
          BEGIN
               P1(-1);
               FAILED ("EXCEPTION NOT RAISED 2A");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
          END;
     
          BEGIN
               P1(0);
               FAILED ("EXCEPTION NOT RAISED 3");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
          END;
     
          BEGIN
               P1(1);
               FAILED ("EXCEPTION NOT RAISED 4");
          EXCEPTION
               WHEN E => NULL;
          END;
     
          BEGIN
               P2(0);
               FAILED ("EXCEPTION NOT RAISED 5");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
          END;
     
          BEGIN
               P2(6);
               FAILED ("EXCEPTION NOT RAISED 6");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
          END;
     
     EXCEPTION
          WHEN OTHERS => FAILED ("WRONG EXCEPTION OR HANDLER");
     END;
     
     RESULT;
EXCEPTION
     WHEN OTHERS => FAILED ("WRONG HANDLER FOR SURE"); RESULT;
END CB4002A;
