-- C95095A.ADA

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
-- CHECK THAT OVERLOADED SUBPROGRAM AND ENTRY DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (A) A FUNCTION AND AN ENTRY.

-- JWC 7/24/85

WITH REPORT; USE REPORT;
PROCEDURE C95095A IS

BEGIN
     TEST ("C95095A", "SUBPROGRAM/ENTRY OVERLOADING WITH " &
                      "MINIMAL DIFFERENCES");

     --------------------------------------------------

     -- BOTH PARAMETERIZED AND PARAMETERLESS SUBPROGRAMS AND ENTRIES
     -- ARE TESTED.

     DECLARE
          I, J, K : INTEGER := 0;
          S : STRING (1..2) := "12";

          TASK T IS
               ENTRY E1 (I1, I2 : INTEGER);
               ENTRY E2;
          END T;

          TASK BODY T IS
          BEGIN
               LOOP
                    SELECT
                         ACCEPT E1 (I1, I2 : INTEGER) DO
                              S (1) := 'A';
                         END E1;
                    OR
                         ACCEPT E2 DO
                              S (1) := 'C';
                         END E2;
                    OR
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END T;

          FUNCTION E1 (I1, I2 : INTEGER) RETURN INTEGER IS
          BEGIN
               S (2) := 'B';
               RETURN I1; -- RETURNED VALUE IS IRRELEVENT.
          END E1;


          FUNCTION E2 RETURN INTEGER IS
          BEGIN
               S (2) := 'D';
               RETURN I; -- RETURNED VALUE IS IRRELEVENT.
          END E2;

     BEGIN
          T.E1 (I, J);
          K := E1 (I, J);

          IF S /= "AB" THEN
               FAILED ("PARAMETERIZED OVERLOADED " &
                       "SUBPROGRAM AND ENTRY " &
                       "CAUSED CONFUSION");
          END IF;

          S := "12";
          T.E2;
          K := E2;

          IF S /= "CD" THEN
               FAILED ("PARAMETERLESS OVERLOADED " &
                       "SUBPROGRAM AND ENTRY " &
                       "CAUSED CONFUSION");
          END IF;
     END;

     --------------------------------------------------

     RESULT;
END C95095A;
