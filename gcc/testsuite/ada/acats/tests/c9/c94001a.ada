-- C94001A.ADA

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
-- CHECK THAT A UNIT WITH DEPENDENT TASKS CREATED BY OBJECT
--   DECLARATIONS IS NOT TERMINATED UNTIL ALL DEPENDENT TASKS BECOME
--   TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A SIMPLE TASK OBJECT, IN A BLOCK.
--   (C, D)  AN ARRAY OF TASK OBJECT, IN A FUNCTION.
--   (E, F)  AN ARRAY OF RECORD OF TASK OBJECT, IN A TASK BODY.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- JRK 10/2/81
-- SPS 11/21/82
-- JRK 11/29/82
-- TBN  8/22/86     REVISED; ADDED CASES THAT EXIT BY RAISING AN
--                  EXCEPTION.
-- PWN 01/31/95     REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94001A IS

     MY_EXCEPTION : EXCEPTION;
     GLOBAL : INTEGER;

     TASK TYPE TT IS
          ENTRY E (I : INTEGER);
     END TT;

     TASK BODY TT IS
          LOCAL : INTEGER;
     BEGIN
          ACCEPT E (I : INTEGER) DO
               LOCAL := I;
          END E;
          DELAY 30.0 * Impdef.One_Second;    -- SINCE THE PARENT UNIT HAS HIGHER PRIORITY
                         -- AT THIS POINT, IT WILL RECEIVE CONTROL AND
                         -- TERMINATE IF THE ERROR IS PRESENT.
          GLOBAL := LOCAL;
     END TT;


BEGIN
     TEST ("C94001A", "CHECK THAT A UNIT WITH DEPENDENT TASKS " &
                      "CREATED BY OBJECT DECLARATIONS IS NOT " &
                      "TERMINATED UNTIL ALL DEPENDENT TASKS " &
                      "BECOME TERMINATED");

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (A)

          T : TT;

     BEGIN -- (A)

          T.E (IDENT_INT(1));

     END; -- (A)

     IF GLOBAL /= 1 THEN
          FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                  "BLOCK EXIT - 1");
     END IF;

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     BEGIN -- (B)
          DECLARE
               T : TT;
          BEGIN
               T.E (IDENT_INT(1));
               RAISE MY_EXCEPTION;
          END;

          FAILED ("MY_EXCEPTION WAS NOT RAISED - 2");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL /= 1 THEN
                    FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                            "BLOCK EXIT - 2");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION - 2");
     END; -- (B)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C)

          I : INTEGER;

          FUNCTION F RETURN INTEGER IS
               A : ARRAY (1..1) OF TT;
          BEGIN
               A(1).E (IDENT_INT(2));
               RETURN 0;
          END F;

     BEGIN -- (C)

          I := F;

          IF GLOBAL /= 2 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "FUNCTION EXIT - 3");
          END IF;

     END; -- (C)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (D)

          I : INTEGER;

          FUNCTION F RETURN INTEGER IS
               A : ARRAY (1..1) OF TT;
          BEGIN
               A(1).E (IDENT_INT(2));
               IF EQUAL (3, 3) THEN
                    RAISE MY_EXCEPTION;
               END IF;
               RETURN 0;
          END F;

     BEGIN -- (D)
          I := F;
          FAILED ("MY_EXCEPTION WAS NOT RAISED - 4");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL /= 2 THEN
                    FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                            "FUNCTION EXIT - 4");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION - 4");
     END; -- (D)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (E)

          LOOP_COUNT : INTEGER := 0;
          CUT_OFF : CONSTANT := 60 * 60;     -- ONE HOUR DELAY.

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS
               TYPE RT IS
                    RECORD
                         T : TT;
                    END RECORD;
               AR : ARRAY (1..1) OF RT;
          BEGIN
               AR(1).T.E (IDENT_INT(3));
          END TSK;

     BEGIN -- (E)

          WHILE NOT TSK'TERMINATED AND LOOP_COUNT < CUT_OFF LOOP
               DELAY 1.0 * Impdef.One_Second;
               LOOP_COUNT := LOOP_COUNT + 1;
          END LOOP;

          IF LOOP_COUNT >= CUT_OFF THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED WITHIN ONE " &
                       "HOUR - 5");
          ELSIF GLOBAL /= 3 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "TASK EXIT - 5");
          END IF;

     END; -- (E)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (F)

          LOOP_COUNT : INTEGER := 0;
          CUT_OFF : CONSTANT := 60 * 60;     -- ONE HOUR DELAY.

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS
               TYPE RT IS
                    RECORD
                         T : TT;
                    END RECORD;
               AR : ARRAY (1..1) OF RT;
          BEGIN
               AR(1).T.E (IDENT_INT(3));
               IF EQUAL (3, 3) THEN
                    RAISE MY_EXCEPTION;
               END IF;
               FAILED ("EXCEPTION WAS NOT RAISED - 6");
          END TSK;

     BEGIN -- (F)

          WHILE NOT TSK'TERMINATED AND LOOP_COUNT < CUT_OFF LOOP
               DELAY 1.0 * Impdef.One_Second;
               LOOP_COUNT := LOOP_COUNT + 1;
          END LOOP;

          IF LOOP_COUNT >= CUT_OFF THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED WITHIN ONE " &
                       "HOUR - 6");
          ELSIF GLOBAL /= 3 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "TASK EXIT - 6");
          END IF;

     END; -- (F)

     --------------------------------------------------

     RESULT;
END C94001A;
