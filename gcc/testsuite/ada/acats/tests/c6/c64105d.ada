-- C64105D.ADA

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
--       (2)
--       (3) BEFORE OR AFTER THE CALL, WHEN AN UNCONSTRAINED ACTUAL 
--           OUT ACCESS PARAMETER DESIGNATES AN OBJECT (PRIOR TO THE
--           CALL) WITH CONSTRAINTS DIFFERENT FROM THE FORMAL
--           PARAMETER.
--   SUBTESTS ARE:
--       (G) CASE 3, STATIC LIMITED PRIVATE DISCRIMINANT.
--       (H) CASE 3, DYNAMIC ONE DIMENSIONAL BOUNDS.

-- JRK 3/20/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C64105D IS

     USE REPORT;

BEGIN
     TEST ("C64105D", "CHECK THAT CONSTRAINT_ERROR IS NOT RAISED " &
           "BEFORE AND AFTER THE CALL, WHEN AN UNCONSTRAINED ACTUAL " &
           "OUT ACCESS PARAMETER DESIGNATES AN OBJECT (PRIOR TO THE " &
           "CALL) WITH CONSTRAINTS DIFFERENT FROM THE FORMAL " &
           "PARAMETER" );

     --------------------------------------------------

     DECLARE -- (G)

          PACKAGE PKG IS
               SUBTYPE INT IS INTEGER RANGE 0..5;
               TYPE T (I : INT := 0) IS LIMITED PRIVATE; 
          PRIVATE          
               TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
               TYPE T (I : INT := 0) IS 
                    RECORD
                         J : INTEGER;
                         A : ARR (1..I);
                    END RECORD;
          END PKG;
          USE PKG;

          TYPE A IS ACCESS T;
          SUBTYPE SA IS A(3);
          V : A := NEW T (2);
          CALLED : BOOLEAN := FALSE;

          PROCEDURE P (X : OUT SA) IS
          BEGIN
               CALLED := TRUE;
               X := NEW T (3);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (G)");
          END P;

     BEGIN -- (G)
 
          P (V);

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (G)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (G)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (G)");
     END; -- (G)

     --------------------------------------------------

     DECLARE -- (H)

          TYPE A IS ACCESS STRING;
          SUBTYPE SA IS A (1..2);
          V : A := NEW STRING (IDENT_INT(5) .. IDENT_INT(7));
          CALLED : BOOLEAN := FALSE;

          PROCEDURE P (X : OUT SA) IS
          BEGIN
               CALLED := TRUE;
               X := NEW STRING (IDENT_INT(1) .. IDENT_INT(2));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE - (H)");
          END P;

     BEGIN -- (H)

          P (V);

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL - (H)");
               ELSE
                    FAILED ("EXCEPTION RAISED ON RETURN - (H)");
               END IF;
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - (H)");
     END; -- (H)

     --------------------------------------------------

     RESULT;
END C64105D;
