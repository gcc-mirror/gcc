-- C94002A.ADA

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
-- CHECK THAT A UNIT WITH DEPENDENT TASKS CREATED BY (LOCAL)
--   ALLOCATORS DOES NOT TERMINATE UNTIL ALL DEPENDENT TASKS ARE
--   TERMINATED.
-- SUBTESTS ARE:
--   (A, B)  A SIMPLE TASK ALLOCATOR, IN A BLOCK.
--   (C, D)  A RECORD OF TASK ALLOCATOR, IN A FUNCTION.
--   (E, F)  A RECORD OF ARRAY OF TASK ALLOCATOR, IN A TASK BODY.

-- THIS TEST CONTAINS SHARED VARIABLES AND RACE CONDITIONS.

-- JRK 10/2/81
-- SPS 11/2/82
-- SPS 11/21/82
-- JRK 11/29/82
-- TBN  8/25/86     REDUCED DELAYS; ADDED LIMITED PRIVATE TYPES; 
--                  INCLUDED EXITS BY RAISING AN EXCEPTION.
-- PWN 01/31/95     REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94002A IS

     PACKAGE P IS
          MY_EXCEPTION : EXCEPTION;
          GLOBAL : INTEGER;
          TASK TYPE T1 IS
               ENTRY E (I : INTEGER);
          END T1;
          TYPE T2 IS LIMITED PRIVATE;
          PROCEDURE CALL_ENTRY (A : T2; B : INTEGER);
     PRIVATE
          TASK TYPE T2 IS
               ENTRY E (I : INTEGER);
          END T2;
     END P;

     PACKAGE BODY P IS
          TASK BODY T1 IS
               LOCAL : INTEGER;
          BEGIN
               ACCEPT E (I : INTEGER) DO
                    LOCAL := I;
               END E;
               DELAY 30.0 * Impdef.One_Second;    -- SINCE THE PARENT UNIT HAS HIGHER
                              -- PRIORITY AT THIS POINT, IT WILL
                              -- RECEIVE CONTROL AND TERMINATE IF
                              -- THE ERROR IS PRESENT.
               GLOBAL := LOCAL;
          END T1;

          TASK BODY T2 IS
               LOCAL : INTEGER;
          BEGIN
               ACCEPT E (I : INTEGER) DO
                    LOCAL := I;
               END E;
               DELAY 30.0 * Impdef.One_Second;
               GLOBAL := LOCAL;
          END T2;

          PROCEDURE CALL_ENTRY (A : T2; B : INTEGER) IS
          BEGIN
               A.E (B);
          END CALL_ENTRY;
     END P;

     USE P;


BEGIN
     TEST ("C94002A", "CHECK THAT A UNIT WITH DEPENDENT TASKS " &
                      "CREATED BY (LOCAL) ALLOCATORS DOES NOT " &
                      "TERMINATE UNTIL ALL DEPENDENT TASKS " &
                      "ARE TERMINATED");

     --------------------------------------------------
     GLOBAL := IDENT_INT (0);
     BEGIN -- (A)
          DECLARE
               TYPE A_T IS ACCESS T1;
               A : A_T;
          BEGIN
               IF EQUAL (3, 3) THEN
                    A := NEW T1;
                    A.ALL.E (IDENT_INT(1));
                    RAISE MY_EXCEPTION;
               END IF;
          END;

          FAILED ("MY_EXCEPTION WAS NOT RAISED - 1");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL /= 1 THEN
                    FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                            "BLOCK EXIT - 1");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
     END; -- (A)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (B)
          TYPE A_T IS ACCESS T2;
          A : A_T;
     BEGIN -- (B)
          IF EQUAL (3, 3) THEN
               A := NEW T2;
               CALL_ENTRY (A.ALL, IDENT_INT(2));
          END IF;
     END; -- (B)

     IF GLOBAL /= 2 THEN
          FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                  "BLOCK EXIT - 2");
     END IF;

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (C)
          I : INTEGER;

          FUNCTION F RETURN INTEGER IS
               TYPE RT;
               TYPE ART IS ACCESS RT;
               TYPE RT IS
                    RECORD
                         A : ART;
                         T : T1;
                    END RECORD;
               LIST : ART;
               TEMP : ART;
          BEGIN
               FOR I IN 1 .. IDENT_INT (1) LOOP
                    TEMP := NEW RT;
                    TEMP.A := LIST;
                    LIST := TEMP;
                    LIST.T.E (IDENT_INT(3));
               END LOOP;
               RETURN 0;
          END F;
     BEGIN -- (C)
          I := F;

          IF GLOBAL /= 3 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "FUNCTION EXIT - 3");
          END IF;
     END; -- (C)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (D)
          I : INTEGER;

          FUNCTION F RETURN INTEGER IS
               TYPE RT;
               TYPE ART IS ACCESS RT;
               TYPE RT IS
                    RECORD
                         A : ART;
                         T : T2;
                    END RECORD;
               LIST : ART;
               TEMP : ART;
          BEGIN
               FOR I IN 1 .. IDENT_INT (1) LOOP
                    TEMP := NEW RT;
                    TEMP.A := LIST;
                    LIST := TEMP;
                    CALL_ENTRY (LIST.T, IDENT_INT(4));
                    IF EQUAL (3, 3) THEN
                         RAISE MY_EXCEPTION;
                    END IF;
               END LOOP;
               RETURN 0;
          END F;
     BEGIN -- (D)
          I := F;

          FAILED ("MY_EXCEPTION WAS NOT RAISED - 4");
     EXCEPTION
          WHEN MY_EXCEPTION =>
               IF GLOBAL /= 4 THEN
                    FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                            "FUNCTION EXIT - 4");
               END IF;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 4");
     END; -- (D)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (E)

          LOOP_COUNT : INTEGER := 0;
          CUT_OFF : CONSTANT := 5 * 60;     -- FIVE MINUTE DELAY.

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS
               TYPE ARR IS ARRAY (1..1) OF T1;
               TYPE RAT;
               TYPE ARAT IS ACCESS RAT;
               TYPE RAT IS
                    RECORD
                         A : ARAT;
                         T : ARR;
                    END RECORD;
               LIST : ARAT;
               TEMP : ARAT;
          BEGIN
               FOR I IN 1 .. IDENT_INT (1) LOOP
                    TEMP := NEW RAT;
                    TEMP.A := LIST;
                    LIST := TEMP;
                    LIST.T(1).E (IDENT_INT(5));
                    IF EQUAL (3, 3) THEN
                         RAISE MY_EXCEPTION;
                    END IF;
               END LOOP;
          END TSK;

     BEGIN -- (E)

          WHILE NOT TSK'TERMINATED AND LOOP_COUNT < CUT_OFF LOOP
               DELAY 1.0 * Impdef.One_Second;
               LOOP_COUNT := LOOP_COUNT + 1;
          END LOOP;

          IF LOOP_COUNT >= CUT_OFF THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED WITHIN FIVE " &
                       "MINUTES - 5");
          END IF;

          IF GLOBAL /= 5 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "TASK EXIT - 5");
          END IF;

     END; -- (E)

     --------------------------------------------------

     GLOBAL := IDENT_INT (0);

     DECLARE -- (F)

          LOOP_COUNT : INTEGER := 0;
          CUT_OFF : CONSTANT := 5 * 60;     -- FIVE MINUTE DELAY.

          TASK TSK IS
               ENTRY ENT;
          END TSK;

          TASK BODY TSK IS
               TYPE ARR IS ARRAY (1..1) OF T2;
               TYPE RAT;
               TYPE ARAT IS ACCESS RAT;
               TYPE RAT IS
                    RECORD
                         A : ARAT;
                         T : ARR;
                    END RECORD;
               LIST : ARAT;
               TEMP : ARAT;
          BEGIN
               FOR I IN 1 .. IDENT_INT (1) LOOP
                    TEMP := NEW RAT;
                    TEMP.A := LIST;
                    LIST := TEMP;
                    CALL_ENTRY (LIST.T(1), IDENT_INT(6));
               END LOOP;
          END TSK;

     BEGIN -- (F)

          WHILE NOT TSK'TERMINATED AND LOOP_COUNT < CUT_OFF LOOP
               DELAY 1.0 * Impdef.One_Second;
               LOOP_COUNT := LOOP_COUNT + 1;
          END LOOP;

          IF LOOP_COUNT >= CUT_OFF THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED WITHIN FIVE " &
                       "MINUTES - 6");
          END IF;

          IF GLOBAL /= 6 THEN
               FAILED ("DEPENDENT TASK NOT TERMINATED BEFORE " &
                       "TASK EXIT - 6");
          END IF;

     END; -- (F)

     RESULT;
END C94002A;
