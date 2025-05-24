-- C92003A.ADA

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
-- CHECK THAT A TASK CAN BE PASSED AS AN ACTUAL IN OR IN OUT PARAMETER
-- IN A SUBPROGRAM CALL AND THAT THE ACTUAL AND FORMAL PARAMETERS DENOTE
-- THE SAME TASK OBJECT.

-- JRK  1/17/81
-- TBN 12/19/85  ADDED IN OUT PARAMETER CASE.
-- PWB  8/04/86  ADDED CHECK THAT FORMAL AND ACTUAL PARAMETERS DENOTE
--               THE SAME TASK OBJECT.

WITH REPORT; USE REPORT;

PROCEDURE C92003A IS

BEGIN

     TEST ("C92003A", "CHECK TASKS PASSED AS ACTUAL IN OR IN OUT " &
                       "PARAMETERS TO SUBPROGRAMS");

     DECLARE

          TASK TYPE TT IS
               ENTRY E (I : INTEGER);
          END TT;

          T, S : TT;

          TASK BODY TT IS
               SOURCE : INTEGER;
          BEGIN

               SELECT
                    ACCEPT E (I : INTEGER) DO
                         SOURCE := I;
                    END E;
               OR
                    TERMINATE;
               END SELECT;

               IF SOURCE /= 1 THEN
                    FAILED ("EXPECTED 1, GOT " & INTEGER'IMAGE(SOURCE));
               END IF;

               SELECT
                    ACCEPT E (I : INTEGER) DO
                         SOURCE := I;
                    END E;
               OR
                    TERMINATE;
               END SELECT;

               IF SOURCE /= 2 THEN
                    FAILED ("EXPECTED 2, GOT " & INTEGER'IMAGE(SOURCE));
               END IF;

               SELECT
                    ACCEPT E (I : INTEGER) DO
                         SOURCE := I;
                    END E;
               OR
                    TERMINATE;
               END SELECT;

               IF SOURCE /= 3 THEN
                    FAILED ("EXPECTED 3, GOT " & INTEGER'IMAGE(SOURCE));
               END IF;

          END TT;

          PROCEDURE P (T : TT) IS
          BEGIN
               T.E(2);
          END P;

          PROCEDURE Q (S : IN OUT TT) IS
          BEGIN
               S.E(2);
          END Q;

     BEGIN

          T.E(1);                  -- FIRST CALL TO T.E
          P(T);                    -- SECOND CALL TO T.E
          T.E(3);                  -- THIRD CALL TO T.E

          S.E(1);                  -- FIRST CALL TO S.E
          Q(S);                    -- SECOND CALL TO S.E
          S.E(3);                  -- THIRD CALL TO S.E

     END;

     RESULT;

END C92003A;
