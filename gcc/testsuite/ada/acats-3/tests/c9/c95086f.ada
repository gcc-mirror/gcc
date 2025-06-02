-- C95086F.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED BEFORE OR AFTER THE ENTRY
-- CALL FOR OUT ARRAY PARAMETERS, WHERE THE ACTUAL PARAMETER HAS THE
-- FORM OF A TYPE CONVERSION.  THE FOLLOWING CASES ARE TESTED:
--   (A) OK CASE.
--   (B) FORMAL CONSTRAINED, BOTH FORMAL AND ACTUAL HAVE SAME NUMBER
--       COMPONENTS PER DIMENSION, BUT ACTUAL INDEX BOUNDS LIE OUTSIDE
--       FORMAL INDEX SUBTYPE.
--   (C) FORMAL CONSTRAINED, FORMAL AND ACTUAL HAVE DIFFERENT NUMBER
--       COMPONENTS PER DIMENSION, BOTH FORMAL AND ACTUAL ARE NULL
--       ARRAYS.
--   (D) FORMAL CONSTRAINED, ACTUAL NULL, WITH INDEX BOUNDS OUTSIDE
--       FORMAL INDEX SUBTYPE.
--   (E) FORMAL UNCONSTRAINED, ACTUAL NULL, WITH INDEX BOUNDS OUTSIDE
--       FORMAL INDEX SUBTYPE FOR NULL DIMENSIONS ONLY.

-- RJW 2/3/86
-- TMB 11/15/95 FIXED INCOMPATIBILITIES WITH ADA95
-- TMB 11/19/96 FIXED SLIDING PROBLEM IN SECTION D

WITH REPORT; USE REPORT;
PROCEDURE C95086F IS

BEGIN
     TEST ("C95086F", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
           "BEFORE OR AFTER THE ENTRY CALL FOR OUT ARRAY PARAMETERS, " &
           "WITH THE ACTUAL HAVING THE FORM OF A TYPE CONVERSION");

     ---------------------------------------------

     DECLARE -- (A)

          SUBTYPE INDEX IS INTEGER RANGE 1..5;
          TYPE ARRAY_TYPE IS ARRAY (INDEX RANGE <>, INDEX RANGE <>)
               OF BOOLEAN;
          SUBTYPE FORMAL IS ARRAY_TYPE (1..3, 1..3);
          SUBTYPE ACTUAL IS ARRAY_TYPE (1..3, 1..3);
          AR : ACTUAL;
          CALLED : BOOLEAN := FALSE;

          TASK T IS
               ENTRY E (X : OUT FORMAL);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (X : OUT FORMAL) DO
                    CALLED := TRUE;
                    X := (1..3 => (1..3 => TRUE));
               END E;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (A)");
          END T;

     BEGIN -- (A)

          T.E (FORMAL (AR));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (A)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (A)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (A)");
     END; -- (A)

     ---------------------------------------------

     DECLARE -- (B)

          SUBTYPE INDEX IS INTEGER RANGE 1..3;
          TYPE FORMAL IS ARRAY (INDEX, INDEX) OF BOOLEAN;
          TYPE ACTUAL IS ARRAY (3..5, 3..5) OF BOOLEAN;
          AR : ACTUAL;
          CALLED : BOOLEAN := FALSE;

          TASK T IS
               ENTRY E (X : OUT FORMAL);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (X : OUT FORMAL) DO
                    CALLED := TRUE;
                    X(3, 3) := TRUE;
               END E;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (B)");
          END T;

     BEGIN -- (B)

          T.E (FORMAL (AR));
          IF AR(5, 5) /= TRUE THEN
               FAILED ("INCORRECT RETURNED VALUE - (B)");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (B)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (B)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (B)");
     END; -- (B)

     ---------------------------------------------

     DECLARE -- (C)

          SUBTYPE INDEX IS INTEGER RANGE 1..5;
          TYPE ARRAY_TYPE IS ARRAY (INDEX RANGE <>, INDEX RANGE <>)
               OF CHARACTER;
          SUBTYPE FORMAL IS ARRAY_TYPE (2..0, 1..3);
          AR : ARRAY_TYPE (2..1, 1..3);
          CALLED : BOOLEAN := FALSE;

          TASK T IS
               ENTRY E (X : OUT FORMAL);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (X : OUT FORMAL) DO
                    IF X'LAST /= 0 AND X'LAST(2) /= 3 THEN
                         FAILED ("WRONG BOUNDS PASSED - (C)");
                    END IF;
                    CALLED := TRUE;
                    X := (2..0 => (1..3 => 'A'));
               END E;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (C)");
          END T;

     BEGIN -- (C)

          T.E (FORMAL (AR));
          IF AR'LAST /= 1 AND AR'LAST(2) /= 3 THEN
               FAILED ("BOUNDS CHANGED - (C)");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (C)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (C)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (C)");
     END; -- (C)

     ---------------------------------------------

     DECLARE -- (D)

          SUBTYPE INDEX IS INTEGER RANGE 1..3;
          TYPE FORMAL IS ARRAY (INDEX RANGE 1..3, INDEX RANGE 3..1)
               OF CHARACTER;
          TYPE ACTUAL IS ARRAY (3..5, 5..3) OF CHARACTER;
          AR : ACTUAL;
          CALLED : BOOLEAN := FALSE;

          TASK T IS
               ENTRY E (X : OUT FORMAL);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (X : OUT FORMAL) DO
                    IF X'LAST /= 3 AND X'LAST(2) /= 1 THEN
                         FAILED ("WRONG BOUNDS PASSED - (D)");
                    END IF;
                    CALLED := TRUE;
                    X := (1..3 => (3..1 => 'A'));
               END E;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (D)");
          END T;

     BEGIN -- (D)

          T.E (FORMAL (AR));
          IF AR'LAST /= 5 AND AR'LAST(2) /= 3 THEN
               FAILED ("BOUNDS CHANGED - (D)");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (D)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (D)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (D)");
     END; -- (D)

     ---------------------------------------------

     DECLARE -- (E)

          SUBTYPE INDEX IS INTEGER RANGE 1..3;
          TYPE FORMAL IS ARRAY (INDEX RANGE <>, INDEX RANGE <>)
               OF CHARACTER;
          TYPE ACTUAL IS ARRAY (POSITIVE RANGE 5..2,
                                POSITIVE RANGE 1..3) OF CHARACTER;
          AR : ACTUAL;
          CALLED : BOOLEAN := FALSE;

          TASK T IS
               ENTRY E (X : OUT FORMAL);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (X : OUT FORMAL) DO
                    IF X'LAST /= 2 AND X'LAST(2) /= 3 THEN
                         FAILED ("WRONG BOUNDS PASSED - (E)");
                    END IF;
                    CALLED := TRUE;
                    X := (3..1 => (1..3 => ' ' ));
               END E;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK - (E)");
          END T;

     BEGIN -- (E)

          T.E (FORMAL (AR));
          IF AR'LAST /= 2 AND AR'LAST(2) /= 3 THEN
               FAILED ("BOUNDS CHANGED - (E)");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (E)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (E)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (E)");
     END; -- (E)

     ---------------------------------------------

     RESULT;
END C95086F;
