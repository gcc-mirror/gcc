-- C64104H.ADA

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

--         (E) AFTER RETURN, IN OUT MODE, STATIC LIMITED PRIVATE
--             DISCRIMINANTS.

-- HISTORY:
--     JRK 03/18/81  CREATED ORIGINAL TEST.
--     NL  10/13/81
--     LB  11/25/86  ADDED CODE TO ENSURE THAT SUBPROGRAMS ARE
--                   ACTUALLY BEING CALLED.
--     BCB 11/12/87  CHANGED HEADER TO STANDARD FORMAT.


WITH REPORT;
PROCEDURE C64104H IS

     USE REPORT;

BEGIN
     TEST ("C64104H", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
           "APPROPRIATELY FOR ACCESS PARAMETERS");

     --------------------------------------------------

     DECLARE

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

          CALLED : BOOLEAN;
          TYPE A IS ACCESS T;

          V : A (2,'B') := NEW T (2,'B');

          PROCEDURE P (X : IN OUT A) IS
          BEGIN
               CALLED := TRUE;
               X := NEW T (2,'A');
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

END C64104H;
