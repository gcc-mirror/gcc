-- C95085H.ADA

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

--       (E) AFTER RETURN, IN OUT MODE, STATIC LIMITED PRIVATE
--           DISCRIMINANTS.

-- JWC 10/23/85

WITH REPORT; USE REPORT;
PROCEDURE C95085H IS

BEGIN
     TEST ("C95085H", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "APPROPRIATELY FOR ACCESS PARAMETERS");

     --------------------------------------------------

     DECLARE

          CALLED : BOOLEAN := FALSE;

          PACKAGE PKG IS
               SUBTYPE INT IS INTEGER RANGE 0..10;
               SUBTYPE CHAR IS CHARACTER RANGE 'A' .. 'C';
               TYPE T (I : INT := 0; C : CHAR := 'A') IS
                    LIMITED PRIVATE;
          PRIVATE
               TYPE T (I : INT := 0; C : CHAR := 'A') IS
                    RECORD
                         J : INTEGER;
                         CASE C IS
                              WHEN 'A' =>
                                   K : INTEGER;
                              WHEN 'B' =>
                                   S : STRING (1..I);
                              WHEN OTHERS =>
                                   NULL;
                         END CASE;
                    END RECORD;
          END PKG;
          USE PKG;

          TYPE A IS ACCESS T;

          V : A (2,'B') := NEW T (2,'B');

          TASK TSK IS
               ENTRY E (X : IN OUT A);
          END TSK;

          TASK BODY TSK IS
          BEGIN
               SELECT
                    ACCEPT E (X : IN OUT A) DO
                         CALLED := TRUE;
                         X := NEW T (2,'A');
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
          FAILED ("EXCEPTION NOT RAISED AFTER RETURN");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED");
     END;

     --------------------------------------------------

     RESULT;
END C95085H;
