-- CB4006A.ADA

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
--     CHECK THAT EXCEPTIONS IN A BLOCK IN A HANDLER
--     ARE HANDLED CORRECTLY.

-- HISTORY:
--     DAT 04/15/81
--     SPS 11/02/82
--     JET 01/06/88  UPDATED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.
--     JRL 05/28/92  CHANGED CODE IN PROGRAM_ERROR BLOCK TO
--                   PREVENT OPTIMIZATION.

WITH REPORT;
USE REPORT;

PROCEDURE CB4006A IS

     I1 : INTEGER RANGE 1 .. 2 := 1;

     PROCEDURE P IS
     BEGIN
          IF EQUAL(3,3) THEN
               RAISE PROGRAM_ERROR;
          END IF;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               DECLARE
                    I : INTEGER RANGE 1 .. 1 := I1;
               BEGIN
                    IF EQUAL(I,I) THEN
                         I := I1 + 1;
                    END IF ;
                    FAILED ("EXCEPTION NOT RAISED 1");

                    IF NOT EQUAL(I,I) THEN
                         COMMENT ("CAN'T OPTIMIZE THIS");
                    END IF;

               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         IF I1 /= 1 THEN
                              FAILED ("WRONG HANDLER 1");
                         ELSE
                              I1 := I1 + 1;
                         END IF;
               END;
          WHEN CONSTRAINT_ERROR =>
               FAILED ("WRONG HANDLER 3");
     END P;

BEGIN
     TEST ("CB4006A", "CHECK THAT EXCEPTIONS IN BLOCKS IN " &
                      "HANDLERS WORK");

     P;
     IF IDENT_INT(I1) /= 2 THEN
          FAILED ("EXCEPTION NOT HANDLED CORRECTLY");
     ELSE
          BEGIN
               P;
               FAILED ("EXCEPTION NOT RAISED CORRECTLY 2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
          END;
     END IF;

     RESULT;

EXCEPTION
     WHEN OTHERS => FAILED ("WRONG HANDLER 2");
          RESULT;

END CB4006A;
