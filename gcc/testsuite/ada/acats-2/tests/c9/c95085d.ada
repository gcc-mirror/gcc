-- C95085D.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE APPROPRIATE
-- CIRCUMSTANCES FOR ACCESS PARAMETERS IN ENTRY CALLS, NAMELY WHEN
-- THE ACTUAL INDEX BOUNDS OR DISCRIMINANTS ARE NOT EQUAL
-- TO THE FORMAL CONSTRAINTS BEFORE THE CALL (FOR IN AND IN OUT
-- MODES), AND WHEN THE FORMAL CONSTRAINTS ARE NOT EQUAL TO THE
-- ACTUAL CONSTRAINTS UPON RETURN (FOR IN OUT AND OUT MODES).

--       (A) BEFORE CALL, IN MODE, STATIC PRIVATE DISCRIMINANT.

-- JWC 10/23/85

WITH REPORT; USE REPORT;
PROCEDURE C95085D IS

BEGIN
     TEST ("C95085D", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "APPROPRIATELY FOR ACCESS PARAMETERS");

     --------------------------------------------------

     DECLARE

          PACKAGE PKG IS
               TYPE E IS (E1, E2, E3);
               TYPE T (D : E := E1) IS PRIVATE;
               TYPE AR IS ARRAY (E1 .. E3) OF INTEGER;
          PRIVATE
               TYPE T (D : E := E1) IS
                    RECORD
                         I : INTEGER;
                         A : AR;
                    END RECORD;
          END PKG;
          USE PKG;

          TYPE A IS ACCESS T;
          SUBTYPE A1 IS A (E3);
          V : A (E2) := NEW T (E2);

          TASK TSK IS
               ENTRY E (X : A1);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (X : A1) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TASK BODY");
          END TSK;

     BEGIN

          TSK.E (V);
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED");
     END;

     ------------------------------------------------

     RESULT;
END C95085D;
