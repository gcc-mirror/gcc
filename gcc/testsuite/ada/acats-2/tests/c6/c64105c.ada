-- C64105C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS NOT RAISED FOR ACCESS PARAMETERS
--   IN THE FOLLOWING CIRCUMSTANCES:
--       (1)
--       (2) AFTER THE CALL, WHEN AN IN OUT OR OUT FORMAL 
--           ACCESS VALUE IS NULL, AND THE ACTUAL PARAMETER HAS 
--           DIFFERENT CONSTRAINTS.
--       (3)
--   SUBTESTS ARE:
--       (C) CASE 2, IN OUT MODE, STATIC PRIVATE DISCRIMINANT.
--       (D) CASE 2, OUT MODE, DYNAMIC TWO DIMENSIONAL BOUNDS.
--       (E) SAME AS (C), WITH TYPE CONVERSION.
--       (F) SAME AS (D), WITH TYPE CONVERSION.

-- JRK 3/20/81
-- SPS 10/26/82
-- CPP 8/8/84

WITH REPORT;
PROCEDURE C64105C IS

     USE REPORT;

BEGIN
     TEST ("C64105C", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
           "AFTER THE CALL, WHEN AN IN OUT OR OUT FORMAL " &
           "ACCESS VALUE IS NULL, AND THE ACTUAL PARAMETER HAS " &
           "DIFFERENT CONSTRAINTS" );

     --------------------------------------------------

     DECLARE -- (C)

          PACKAGE PKG IS
               TYPE E IS (E1, E2);
               TYPE T (D : E := E1) IS PRIVATE;
          PRIVATE
               TYPE T (D : E := E1) IS
                    RECORD
                         I : INTEGER;
                         CASE D IS
                              WHEN E1 =>
                                   B : BOOLEAN;
                              WHEN E2 =>
                                   C : CHARACTER;
                         END CASE;
                    END RECORD;
          END PKG;
          USE PKG;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A(E2);
          V : A (E1) := NULL;
          ENTERED : BOOLEAN := FALSE;

          PROCEDURE P (X : IN OUT SA) IS
          BEGIN
               ENTERED := TRUE;
               X := NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (C)");
          END P;

     BEGIN -- (C)

          P (V);

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT ENTERED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (C)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (C)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (C)");
     END; -- (C)

     --------------------------------------------------

     DECLARE -- (D)

          TYPE T IS ARRAY (CHARACTER RANGE <>, BOOLEAN RANGE <>) OF
                    INTEGER;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A ('D'..'F', FALSE..FALSE);
          V : A (IDENT_CHAR('A') .. IDENT_CHAR('B'),
                 IDENT_BOOL(TRUE) .. IDENT_BOOL(TRUE)) := NULL;
          ENTERED : BOOLEAN := FALSE;

          PROCEDURE P (X : OUT SA) IS
          BEGIN
               ENTERED := TRUE;
               X := NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (D)");
          END P;

     BEGIN -- (D)

          P (V);

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT ENTERED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (D)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (D)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (D)");
     END; -- (D)

     --------------------------------------------------

     DECLARE -- (E)

          PACKAGE PKG IS
               TYPE E IS (E1, E2);
               TYPE T (D : E := E1) IS PRIVATE;
          PRIVATE
               TYPE T (D : E := E1) IS
                    RECORD
                         I : INTEGER;
                         CASE D IS
                              WHEN E1 =>
                                   B : BOOLEAN;
                              WHEN E2 =>
                                   C : CHARACTER;
                         END CASE;
                    END RECORD;
          END PKG;
          USE PKG;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A(E2);
          V : A (E1) := NULL;
          ENTERED : BOOLEAN := FALSE;

          PROCEDURE P (X : IN OUT SA) IS
          BEGIN
               ENTERED := TRUE;
               X := NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (C)");
          END P;

     BEGIN -- (E)

          P (SA(V));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT ENTERED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (E)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (E)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (E)");
     END; -- (E)

     --------------------------------------------------

     DECLARE -- (F)

          TYPE T IS ARRAY (CHARACTER RANGE <>, BOOLEAN RANGE <>) OF
                    INTEGER;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A ('D'..'F', FALSE..FALSE);
          V : A (IDENT_CHAR('A') .. IDENT_CHAR('B'),
                 IDENT_BOOL(TRUE) .. IDENT_BOOL(TRUE)) := NULL;
          ENTERED : BOOLEAN := FALSE;

          PROCEDURE P (X : OUT SA) IS
          BEGIN
               ENTERED := TRUE;
               X := NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (D)");
          END P;

     BEGIN -- (D)

          P (SA(V));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT ENTERED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (F)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (F)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (F)");
     END; -- (F)

     --------------------------------------------------

     RESULT;
END C64105C;
