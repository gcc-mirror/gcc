-- CB4003A.ADA

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
--     CHECK THAT EXCEPTIONS RAISED DURING ELABORATION OF PACKAGE
--     SPECIFICATIONS, OR DECLARATIVE_PARTS OF BLOCKS AND PACKAGE
--     BODIES, ARE PROPAGATED TO THE STATIC ENVIRONMENT.  EXCEPTIONS
--     ARE CAUSED BY INITIALIZATIONS AND FUNCTION CALLS.

-- HISTORY:
--     DAT 04/14/81  CREATED ORIGINAL TEST.
--     JET 01/06/88  UPDATED HEADER FORMAT AND ADDED CODE TO
--                   PREVENT OPTIMIZATION.

WITH REPORT; USE REPORT;

PROCEDURE CB4003A IS

     E : EXCEPTION;

     FUNCTION F (B : BOOLEAN) RETURN INTEGER IS
     BEGIN
          IF B THEN
               RAISE E;
          ELSE
               RETURN 1;
          END IF;
     END F;

BEGIN
     TEST ("CB4003A", "CHECK THAT EXCEPTIONS DURING ELABORATION"
          & " OF DECLARATIVE PARTS"
          & " IN BLOCKS, PACKAGE SPECS, AND PACKAGE BODIES ARE"
          & " PROPAGATED TO STATIC ENCLOSING ENVIRONMENT");

     BEGIN
          DECLARE
               PACKAGE P1 IS
                    I : INTEGER RANGE 1 .. 1 := 2;
               END P1;
          BEGIN
               FAILED ("EXCEPTION NOT RAISED 1");
               IF NOT EQUAL(P1.I,P1.I) THEN
                    COMMENT ("NO EXCEPTION RAISED");
               END IF;
          EXCEPTION
               WHEN OTHERS => FAILED ("WRONG HANDLER 1");
          END;
          FAILED ("EXCEPTION NOT RAISED 1A");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>NULL;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION 1");
     END;

     FOR L IN IDENT_INT(1) .. IDENT_INT(4) LOOP
          BEGIN
               DECLARE
                    PACKAGE P2 IS
                    PRIVATE
                         J : INTEGER RANGE 2 .. 4 := L;
                    END P2;

                    Q : INTEGER := F(L = 3);

                    PACKAGE BODY P2 IS
                         K : INTEGER := F(L = 2);

                    BEGIN
                         IF NOT (EQUAL(J,J) OR EQUAL(K,K)) THEN
                              COMMENT("CAN'T OPTIMIZE THIS");
                         END IF;
                    END P2;
               BEGIN
                    IF L /= 4 THEN
                         FAILED ("EXCEPTION NOT RAISED 2");
                    END IF;

                    IF NOT EQUAL(Q,Q) THEN
                         COMMENT("CAN'T OPTIMIZE THIS");
                    END IF;

                    EXIT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION HANDLER 2");
                         EXIT;
               END;
               FAILED ("EXCEPTION NOT RAISED 2A");
          EXCEPTION
               WHEN E | CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED 2");
          END;
     END LOOP;

     RESULT;

END CB4003A;
