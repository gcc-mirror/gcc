-- C95086B.ADA

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
--   BEFORE AN ENTRY CALL, WHEN AN IN OR IN OUT ACTUAL ACCESS
--   PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS DIFFERENT
--   FROM THE FORMAL PARAMETER.
--
--   SUBTESTS ARE:
--       (A) IN MODE, STATIC ONE DIMENSIONAL BOUNDS.
--       (B) IN OUT MODE, DYNAMIC RECORD DISCRIMINANTS.
--       (C) CASE (A), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.
--       (D) CASE (B), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.

-- RJW 1/27/86

WITH REPORT; USE REPORT;
PROCEDURE C95086B IS

BEGIN
     TEST ( "C95086B", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
            "BEFORE AN ENTRY CALL, WHEN AN IN OR IN OUT ACTUAL " &
            "ACCESS PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS " &
            "DIFFERENT FROM THE FORMAL PARAMETER" );

     --------------------------------------------------

     DECLARE -- (A)

          TYPE E IS (E1, E2, E3, E4);
          TYPE T IS ARRAY (E RANGE <>) OF INTEGER;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A (E2..E4);
          V : A (E1..E2) := NULL;

          TASK T1 IS
               ENTRY P (X : SA);
          END T1;

          TASK BODY T1 IS
          BEGIN
               ACCEPT P (X : SA);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED IN TASK - (A)" );
          END T1;

     BEGIN -- (A)

          T1.P (V);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - (A)" );
     END; -- (A)

     --------------------------------------------------

     DECLARE -- (B)

          TYPE ARR IS ARRAY (CHARACTER RANGE <>) OF INTEGER;

          TYPE T (B : BOOLEAN := FALSE; C : CHARACTER := 'A') IS
               RECORD
                    I : INTEGER;
                    CASE B IS
                         WHEN FALSE =>
                              J : INTEGER;
                         WHEN TRUE =>
                              A : ARR ('A' .. C);
                    END CASE;
               END RECORD;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A (TRUE, 'C');
          V : A (IDENT_BOOL(FALSE), IDENT_CHAR('B')) := NULL;

          TASK T1 IS
               ENTRY P (X : IN OUT SA);
          END T1;

          TASK BODY T1 IS
          BEGIN
               ACCEPT P (X : IN OUT SA) DO
                    NULL;
               END P;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED IN TASK - (B)" );
          END T1;

     BEGIN -- (B)

          T1.P (V);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - (B)" );
     END; -- (B)

     --------------------------------------------------

     DECLARE -- (C)

          TYPE E IS (E1, E2, E3, E4);
          TYPE T IS ARRAY (E RANGE <>) OF INTEGER;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A (E2..E4);
          V : A (E1..E2) := NULL;

          TASK T1 IS
               ENTRY P (X : SA);
          END T1;

          TASK BODY T1 IS
          BEGIN
               ACCEPT P (X : SA) DO
                    NULL;
               END P;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED IN TASK - (C)" );
          END T1;

     BEGIN -- (C)

          T1.P (SA(V));

     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - (C)" );
     END; -- (C)

     --------------------------------------------------

     DECLARE -- (D)

          TYPE ARR IS ARRAY (CHARACTER RANGE <>) OF INTEGER;

          TYPE T (B : BOOLEAN := FALSE; C : CHARACTER := 'A') IS
               RECORD
                    I : INTEGER;
                    CASE B IS
                         WHEN FALSE =>
                              J : INTEGER;
                         WHEN TRUE =>
                              A : ARR ('A' .. C);
                    END CASE;
               END RECORD;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A (TRUE, 'C');
          V : A (IDENT_BOOL(FALSE), IDENT_CHAR('B')) := NULL;

          TASK T1 IS
               ENTRY P (X : IN OUT SA);
          END T1;

          TASK BODY T1 IS
          BEGIN
               ACCEPT P (X : IN OUT SA);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED IN TASK - (D)" );
          END T1;

     BEGIN -- (D)

          T1.P (SA(V));

     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - (D)" );
     END; -- (D)

     --------------------------------------------------

     RESULT;
END C95086B;
