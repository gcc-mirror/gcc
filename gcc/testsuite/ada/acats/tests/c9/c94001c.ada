-- C94001C.ADA

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
-- CHECK THAT A UNIT WITH INDIRECT DEPENDENT TASKS CREATED BY OBJECT
-- DECLARATIONS IS NOT TERMINATED UNTIL ALL INDIRECT DEPENDENT TASKS
-- BECOME TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A BLOCK CONTAINING A SIMPLE TASK OBJECT, IN A BLOCK.
--   (C, D)  A FUNCTION CONTAINING AN ARRAY OF TASK OBJECT, IN A
--           FUNCTION.
--   (E, F)  A TASK CONTAINING AN ARRAY OF RECORD OF TASK OBJECT,
--           IN A TASK BODY.
--   CASES (B, D, F) EXIT BY RAISING AN EXCEPTION.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- TBN  8/25/86
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94001C IS

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
     TEST ("C94001C", "CHECK THAT A UNIT WITH INDIRECT DEPENDENT " &
                      "TASKS CREATED BY OBJECT DECLARATIONS IS NOT " &
                      "TERMINATED UNTIL ALL INDIRECT DEPENDENT TASKS " &
                      "BECOME TERMINATED");

     --------------------------------------------------
     GLOBAL := IDENT_INT (0);

     BEGIN -- (A)

          DECLARE
               T : TT;
          BEGIN
               T.E (IDENT_INT(1));
          END;

     END; -- (A)

     IF GLOBAL /= 1 THEN
          FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                  "BLOCK EXIT - 1");
     END IF;

     --------------------------------------------------

     BEGIN -- (B)
          GLOBAL := IDENT_INT (0);

          BEGIN
               DECLARE
                    T : TT;
               BEGIN
                    T.E (IDENT_INT(2));
                    RAISE MY_EXCEPTION;
               END;
          END;

          FAILED ("MY_EXCEPTION WAS NOT RAISED - 2");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL /= 2 THEN
                    FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                            "BLOCK EXIT - 2");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION - 2");
     END; -- (B)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C)

          OBJ_INT : INTEGER;

          FUNCTION F1 RETURN INTEGER IS
               I : INTEGER;

               FUNCTION F2 RETURN INTEGER IS
                    A : ARRAY (1..1) OF TT;
               BEGIN
                    A(1).E (IDENT_INT(3));
                    RETURN 0;
               END F2;
          BEGIN
               I := F2;
               RETURN (0);
          END F1;

     BEGIN -- (C)
          OBJ_INT := F1;
          IF GLOBAL /= 3 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "FUNCTION EXIT - 3");
          END IF;
     END; -- (C)

     --------------------------------------------------

     DECLARE -- (D)

          OBJ_INT : INTEGER;

          FUNCTION F1 RETURN INTEGER IS
               I : INTEGER;

               FUNCTION F2 RETURN INTEGER IS
                    A : ARRAY (1..1) OF TT;
               BEGIN
                    A(1).E (IDENT_INT(4));
                    IF EQUAL (3, 3) THEN
                         RAISE MY_EXCEPTION;
                    END IF;
                    RETURN 0;
               END F2;
          BEGIN
               I := F2;
               RETURN (0);
          END F1;

     BEGIN -- (D)
          GLOBAL := IDENT_INT (0);
          OBJ_INT := F1;
          FAILED ("MY_EXCEPTION WAS NOT RAISED - 4");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL /= 4 THEN
                    FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                            "FUNCTION EXIT - 4");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION - 4");
     END; -- (D)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (E)
          DELAY_COUNT : INTEGER := 0;
          TASK OUT_TSK;

          TASK BODY OUT_TSK IS

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
                    AR(1).T.E (IDENT_INT(5));
               END TSK;

          BEGIN
               NULL;
          END OUT_TSK;

     BEGIN -- (E)
          WHILE NOT(OUT_TSK'TERMINATED) AND DELAY_COUNT < 60 LOOP
               DELAY 1.0 * Impdef.One_Long_Second;
               DELAY_COUNT := DELAY_COUNT + 1;
          END LOOP;
          IF DELAY_COUNT = 60 THEN
               FAILED ("OUT_TSK HAS NOT TERMINATED - 5");
          ELSIF GLOBAL /= 5 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "BLOCK EXIT - 5");
          END IF;
     END; -- (E)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE
          DELAY_COUNT : INTEGER := 0;

          TASK OUT_TSK;
     
          TASK BODY OUT_TSK IS

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
                    AR(1).T.E (IDENT_INT(6));
                    RAISE MY_EXCEPTION;
               END TSK;

          BEGIN
               RAISE MY_EXCEPTION;
          END OUT_TSK;

     BEGIN
          WHILE NOT(OUT_TSK'TERMINATED) AND DELAY_COUNT < 60 LOOP
               DELAY 1.0 * Impdef.One_Long_Second;
               DELAY_COUNT := DELAY_COUNT + 1;
          END LOOP;
          IF DELAY_COUNT = 60 THEN
               FAILED ("OUT_TSK HAS NOT TERMINATED - 6");
          ELSIF GLOBAL /= 6 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "BLOCK EXIT - 6");
          END IF;
     END;

     RESULT;
END C94001C;
