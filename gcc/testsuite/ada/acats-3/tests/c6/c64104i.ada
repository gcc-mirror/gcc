-- C64104I.ADA

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
-- OBJECTIVE:
--     CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER THE APPROPRIATE
--     CIRCUMSTANCES FOR ACCESS PARAMETERS, NAMELY WHEN THE
--     ACTUAL INDEX BOUNDS OR DISCRIMINANTS ARE NOT EQUAL
--     TO THE FORMAL CONSTRAINTS BEFORE THE CALL (FOR IN AND IN OUT
--     MODES), AND WHEN THE FORMAL CONSTRAINTS ARE NOT EQUAL TO THE
--     ACTUAL CONSTRAINTS UPON RETURN (FOR IN OUT AND OUT MODES).

--         (F) AFTER RETURN, IN OUT MODE, DYNAMIC THREE DIMENSIONAL
--             BOUNDS.

-- HISTORY:
--     JRK 03/18/81  CREATED ORIGINAL TEST.
--     NL  10/13/81
--     LB  11/25/86  ADDED CODE TO ENSURE THAT SUBPROGRAMS ARE
--                   ACTUALLY BEING CALLED.
--     BCB 11/12/87  CHANGED HEADER TO STANDARD FORMAT.


WITH REPORT;
PROCEDURE C64104I IS

     USE REPORT;

BEGIN
     TEST ("C64104I", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
           "APPROPRIATELY FOR ACCESS PARAMETERS");

     --------------------------------------------------

     DECLARE

          CALLED : BOOLEAN;

          TYPE E IS (E1, E2, E3);

          TYPE T IS ARRAY (CHARACTER RANGE <>,
                           E RANGE <>,
                           BOOLEAN RANGE <>
                          ) OF INTEGER;

          TYPE A IS ACCESS T;

          V : A ('A'..'Z', E1..E2, BOOLEAN) :=
                    NEW T ('A'..'Z', E1..E2, BOOLEAN);

          PROCEDURE P (X : IN OUT A) IS
          BEGIN
               CALLED := TRUE;
               IF EQUAL (3,3) THEN
                    X := NEW T ('A'..'Z', E2..E3, BOOLEAN);
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN PROCEDURE");
          END P;

     BEGIN

          CALLED := FALSE;
          P (V);
          FAILED ("EXCEPTION NOT RAISED AFTER RETURN");

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF NOT CALLED THEN
                    FAILED ("SUBPROGRAM P WAS NOT CALLED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED");
     END;

     --------------------------------------------------

     RESULT;

END C64104I;
