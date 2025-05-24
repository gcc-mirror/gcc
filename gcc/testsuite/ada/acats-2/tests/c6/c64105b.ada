-- C64105B.ADA

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
--       (1) BEFORE THE CALL, WHEN AN IN OR IN OUT ACTUAL ACCESS
--           PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS DIFFERENT 
--           FROM THE FORMAL PARAMETER.
--       (2)
--       (3)
--   SUBTESTS ARE:
--       (A) CASE 1, IN MODE, STATIC ONE DIMENSIONAL BOUNDS.
--       (B) CASE 1, IN OUT MODE, DYNAMIC RECORD DISCRIMINANTS.
--       (C) CASE (A), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.
--       (D) CASE (B), BUT ACTUAL PARAMETER IS A TYPE CONVERSION.

-- JRK 3/20/81
-- SPS 10/26/82
-- CPP 8/6/84

WITH REPORT;
PROCEDURE C64105B IS

     USE REPORT;

BEGIN
     TEST ("C64105B", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
           "BEFORE THE CALL, WHEN AN IN OR IN OUT ACTUAL ACCESS " &
           "PARAMETER HAS VALUE NULL, BUT WITH CONSTRAINTS DIFFERENT " &
           "FROM THE FORMAL PARAMETER" );

     --------------------------------------------------

     DECLARE -- (A)

          TYPE E IS (E1, E2, E3, E4);
          TYPE T IS ARRAY (E RANGE <>) OF INTEGER;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A(E2..E4);
          V : A (E1..E2) := NULL;

          PROCEDURE P (X : SA ) IS
          BEGIN
               NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (A)");
          END P;
     
     BEGIN -- (A)

          P (V);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (A)");
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
          SUBTYPE SA IS A(TRUE, 'C');
          V : A (IDENT_BOOL(FALSE), IDENT_CHAR('B')) := NULL;

          PROCEDURE P (X : IN OUT SA ) IS
          BEGIN
               NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (B)");
          END P;

     BEGIN -- (B)

          P (V);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (B)");
     END; -- (B)

     --------------------------------------------------

     DECLARE -- (C)

          TYPE E IS (E1, E2, E3, E4);
          TYPE T IS ARRAY (E RANGE <>) OF INTEGER;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A(E2..E4);
          V : A (E1..E2) := NULL;

          PROCEDURE P (X : SA ) IS
          BEGIN
               NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (C)");
          END P;
     
     BEGIN -- (C)

          P (SA(V));

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (C)");
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
          SUBTYPE SA IS A(TRUE, 'C');
          V : A (IDENT_BOOL(FALSE), IDENT_CHAR('B')) := NULL;

          PROCEDURE P (X : IN OUT SA ) IS
          BEGIN
               NULL;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (D)");
          END P;

     BEGIN -- (D)

          P (SA(V));

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (D)");
     END; -- (D)

     --------------------------------------------------

     RESULT;
END C64105B;
