-- C64104C.ADA

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
--   APPROPRIATE CIRCUMSTANCES FOR ARRAY PARAMETERS, NAMELY
--   WHEN THE ACTUAL BOUNDS DON'T MATCH THE FORMAL BOUNDS
--   (BEFORE THE CALL FOR ALL MODES).
--   SUBTESTS ARE:
--      (A) IN MODE, ONE DIMENSION, STATIC AGGREGATE.
--      (B) IN MODE, TWO DIMENSIONS, DYNAMIC AGGREGATE.
--      (C) IN MODE, TWO DIMENSIONS, DYNAMIC VARIABLE.
--      (D) IN OUT MODE, THREE DIMENSIONS, STATIC VARIABLE.
--      (E) OUT MODE, ONE DIMENSION, DYNAMIC VARIABLE.
--      (F) IN OUT MODE, NULL STRING AGGREGATE.
--      (G) IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE (OK CASE).
--          IN OUT MODE, TWO DIMENSIONS, NULL AGGREGATE.

-- JRK 3/17/81
-- SPS 10/26/82
-- CPP 8/6/84
-- PWN 11/30/94 REMOVED TEST ILLEGAL IN ADA 9X.

WITH REPORT;
PROCEDURE C64104C IS

     USE REPORT;

BEGIN
     TEST ("C64104C", "CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN " &
           "ACTUAL ARRAY BOUNDS DON'T MATCH FORMAL BOUNDS");

     --------------------------------------------------

     DECLARE -- (A)
          SUBTYPE ST IS STRING (1..3);

          PROCEDURE P (A : ST) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED ON CALL - (A)");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (A)");
          END P;

     BEGIN -- (A)

          P ("AB");
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

          PROCEDURE P (A : T) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED ON CALL - (B)");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (B)");
          END P;

     BEGIN -- (B)

          P ((1..3 => (1..IDENT_INT(2) => 0)));
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

          PROCEDURE P (A :ST) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED ON CALL - (C)");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (C)");
          END P;

     BEGIN -- (C)

          P (V);
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

          PROCEDURE P (A : IN OUT ST) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED ON CALLL - (D)");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (D)");
          END P;

     BEGIN -- (D)

          P (V);
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

          PROCEDURE P (A : IN OUT ST) IS
          BEGIN
               COMMENT ("OK CASE CALLED CORRECTLY");
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (G)");
          END P;

     BEGIN -- (G)

          P (V);

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED ON OK CASE - (G)");
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED ON OK CASE - (G)");
     END; -- (G)

     --------------------------------------------------

     --------------------------------------------------

     RESULT;
END C64104C;
