-- C95095B.ADA

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
-- CHECK THAT OVERLOADED ENTRY DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (B) ONE ENTRY HAS ONE LESS PARAMETER THAN THE OTHER.

-- JWC 7/24/85
-- JRK 10/2/85

WITH REPORT; USE REPORT;
PROCEDURE C95095B IS

BEGIN
     TEST ("C95095B", "ENTRY OVERLOADING WITH " &
                      "MINIMAL DIFFERENCES");

     --------------------------------------------------

     -- ONE ENTRY HAS ONE MORE PARAMETER
     -- THAN THE OTHER.  THIS IS TESTED IN THE
     -- CASE IN WHICH THAT PARAMETER HAS A DEFAULT
     -- VALUE, AND THE CASE IN WHICH IT DOES NOT.

     DECLARE
          I, J : INTEGER := 0;
          B : BOOLEAN := TRUE;
          S : STRING (1..2) := "12";

          TASK T IS
               ENTRY E1 (I1, I2 : INTEGER; B1 : IN OUT BOOLEAN);
               ENTRY E1 (I1, I2 : INTEGER);
               ENTRY E2 (B1 : IN OUT BOOLEAN; I1 : INTEGER := 0);
               ENTRY E2 (B1 : IN OUT BOOLEAN);
          END T;

          TASK BODY T IS
          BEGIN
               LOOP
                    SELECT
                         ACCEPT E1 (I1, I2 : INTEGER;
                                    B1 : IN OUT BOOLEAN) DO
                              S (1) := 'A';
                         END E1;
                    OR
                         ACCEPT E1 (I1, I2 : INTEGER) DO
                              S (2) := 'B';
                         END E1;
                    OR
                         ACCEPT E2 (B1 : IN OUT BOOLEAN;
                                    I1 : INTEGER := 0) DO
                              S (1) := 'C';
                         END E2;
                    OR
                         ACCEPT E2 (B1 : IN OUT BOOLEAN) DO
                              S (2) := 'D';
                         END E2;
                    OR
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END T;

     BEGIN
          T.E1 (I, J, B);
          T.E1 (I, J);

          IF S /= "AB" THEN
               FAILED ("ENTRIES DIFFERING ONLY IN " &
                       "NUMBER OF PARAMETERS (NO DEFAULTS) " &
                       "CAUSED CONFUSION");
          END IF;

          S := "12";
          T.E2 (B, I);
          -- NOTE THAT A CALL TO T.E2 WITH ONLY
          -- ONE PARAMETER IS AMBIGUOUS.

          IF S /= "C2" THEN
               FAILED ("ENTRIES DIFFERING ONLY IN " &
                       "EXISTENCE OF ONE PARAMETER (WITH " &
                       "DEFAULT) CAUSED CONFUSION");
          END IF;
     END;

     --------------------------------------------------

     RESULT;
END C95095B;
