-- C95085C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE
-- APPROPRIATE CIRCUMSTANCES FOR ARRAY PARAMETERS IN ENTRY CALLS,
-- NAMELY WHEN THE ACTUAL BOUNDS DON'T MATCH THE FORMAL BOUNDS
-- (BEFORE THE CALL FOR ALL MODES).
-- SUBTESTS ARE:
--      (A) IN MODE, ONE DIMENSION, STATIC AGGREGATE.
--      (B) IN MODE, TWO DIMENSIONS, DYNAMIC AGGREGATE.
--      (C) IN MODE, TWO DIMENSIONS, DYNAMIC VARIABLE.
--      (D) IN OUT MODE, THREE DIMENSIONS, STATIC VARIABLE.
--      (E) OUT MODE, ONE DIMENSION, DYNAMIC VARIABLE.
--      (F) IN OUT MODE, NULL STRING AGGREGATE.
--      (G) IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE (OK CASE).
--          IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE.

-- JWC 10/28/85
-- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X.

WITH REPORT; USE REPORT;
PROCEDURE C95085C IS

BEGIN
     TEST ("C95085C", "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                      "ACTUAL ARRAY BOUNDS DON'T MATCH FORMAL BOUNDS");

     --------------------------------------------------

     DECLARE -- (A)
          SUBTYPE ST IS STRING (1..3);

          TASK TSK IS
               ENTRY E (A : ST);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (A : ST) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL - (A)");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (A)");
          END TSK;

     BEGIN -- (A)

          TSK.E ("AB");
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL - (A)");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - (A)");
     END; -- (A)

     --------------------------------------------------

     DECLARE -- (B)

          SUBTYPE S IS INTEGER RANGE 1..3;
          TYPE T IS ARRAY (S,S) OF INTEGER;

          TASK TSK IS
               ENTRY E (A : T);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (A : T) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL - (B)");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (B)");
          END TSK;

     BEGIN -- (B)

          TSK.E ((1..3 => (1..IDENT_INT(2) => 0)));
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL - (B)");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - (B)");
     END; -- (B)

     --------------------------------------------------

     DECLARE -- (C)

          SUBTYPE S IS INTEGER RANGE 1..5;
          TYPE T IS ARRAY (S RANGE <>, S RANGE <>) OF INTEGER;
          SUBTYPE ST IS T (1..3,1..3);
          V : T (1..IDENT_INT(2), 1..3) :=
                (1..IDENT_INT(2) => (1..3 => 0));

          TASK TSK IS
               ENTRY E (A :ST);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (A :ST) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL - (C)");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (C)");
          END TSK;

     BEGIN -- (C)

          TSK.E (V);
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL - (C)");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - (C)");
     END; -- (C)

     --------------------------------------------------

     DECLARE -- (D)

          SUBTYPE S IS INTEGER RANGE 1..5;
          TYPE T IS ARRAY (S RANGE <>, S RANGE <>, S RANGE <>) OF
                    INTEGER;
          SUBTYPE ST IS T (1..3, 1..3, 1..3);
          V : T (1..3, 1..2, 1..3) :=
                (1..3 => (1..2 => (1..3 => 0)));

          TASK TSK IS
               ENTRY E (A : IN OUT ST);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (A : IN OUT ST) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL - (D)");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (D)");
          END TSK;

     BEGIN -- (D)

          TSK.E (V);
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL - (D)");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - (D)");
     END; -- (D)

     --------------------------------------------------


     DECLARE -- (G)

          SUBTYPE S IS INTEGER RANGE 1..5;
          TYPE T IS ARRAY (S RANGE <>, S RANGE <>) OF CHARACTER;
          SUBTYPE ST IS T (2..1, 2..1);
          V : T (2..1, 2..1) := (2..1 => (2..1 => ' '));

          TASK TSK IS
               ENTRY E (A : IN OUT ST);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (A : IN OUT ST)  DO
                         COMMENT ("OK CASE CALLED CORRECTLY");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (G)");
          END TSK;

     BEGIN -- (G)

          TSK.E (V);

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED ON OK CASE - (G)");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED ON OK CASE - (G)");
     END; -- (G)

     --------------------------------------------------


     RESULT;
END C95085C;
