-- C64104M.ADA

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
--   CIRCUMSTANCES FOR ACCESS PARAMETERS, NAMELY WHEN THE
--   ACTUAL INDEX BOUNDS OR DISCRIMINANTS ARE NOT EQUAL
--   TO THE FORMAL CONSTRAINTS BEFORE THE CALL (FOR IN AND IN OUT
--   MODES), AND WHEN THE FORMAL CONSTRAINTS ARE NOT EQUAL TO THE 
--   ACTUAL CONSTRAINTS UPON RETURN (FOR IN OUT AND OUT MODES).

--       (J) AFTER RETURN, OUT MODE, CONSTRAINED FORMAL, DYNAMIC TWO
--           DIMENSIONAL BOUNDS.

-- JRK 3/18/81
-- NL 10/13/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C64104M IS

     USE REPORT;

BEGIN
     TEST ("C64104M", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
           "APPROPRIATELY FOR ACCESS PARAMETERS");

     --------------------------------------------------

     DECLARE

          TYPE T IS ARRAY (INTEGER RANGE <>,
                           CHARACTER RANGE <>
                          ) OF INTEGER;

          TYPE A IS ACCESS T;

          V : A (1..10, 'A'..'Z') := NEW T (1..10, 'A'..'Z');

          ENTERED : BOOLEAN := FALSE;
          Y : CONSTANT CHARACTER := IDENT_CHAR('Y');
          SUBTYPE SA IS A(1..10, 'A'..Y);
          PROCEDURE P (X : OUT SA ) IS
          BEGIN
               ENTERED := TRUE;
               X := NEW T (1..10, 'A'..IDENT_CHAR('Y'));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE");
          END P;

     BEGIN

          P (V);
          FAILED ("EXCEPTION NOT RAISED AFTER RETURN");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT ENTERED THEN
                    FAILED ("CONSTRAINT_ERROR RAISED BEFORE " & 
                            "CALL");
               END IF;
          WHEN OTHERS =>
               IF NOT ENTERED THEN
                    FAILED ("OTHER EXCEPTION RAISED BEFORE CALL");
               ELSE FAILED ("WRONG EXCEPTION RAISED AFTER " & 
                            "RETURN");
               END IF;
     END;

     --------------------------------------------------

     RESULT;

END C64104M;
