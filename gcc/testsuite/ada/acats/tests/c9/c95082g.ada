-- C95082G.ADA

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
-- CHECK THAT FOR CALLS TO ENTRIES HAVING AT LEAST ONE DEFAULT
-- PARAMETER, THE CORRECT ASSOCIATION IS MADE BETWEEN ACTUAL AND
-- FORMAL PARAMETERS.

-- JWC 7/17/85

WITH REPORT;USE REPORT;
PROCEDURE C95082G IS

     Y1,Y2,Y3  : INTEGER := 0;

     TASK T IS
          ENTRY E (I1: INTEGER; I2: INTEGER := 2; I3: INTEGER := 3;
                   O1,O2,O3: OUT INTEGER);
     END T;

     TASK BODY T IS
     BEGIN
          LOOP
               SELECT
                    ACCEPT E (I1: INTEGER; I2: INTEGER := 2;
                              I3: INTEGER := 3;
                              O1,O2,O3: OUT INTEGER) DO
                         O1 := I1;
                         O2 := I2;
                         O3 := I3;
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          END LOOP;
     END T;


BEGIN

     TEST ("C95082G", "CHECK ASSOCIATIONS BETWEEN ACTUAL AND FORMAL " &
                      "PARAMETERS (HAVING DEFAULT VALUES)");

     T.E (I1=>11, I2=>12, I3=>13, O1=>Y1, O2=>Y2, O3=>Y3);
     IF (Y1 /= 11) OR (Y2 /= 12) OR (Y3 /= 13) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 1");
     END IF;

     T.E (I1=>21, O1=>Y1, O2=>Y2, O3=>Y3);
     IF (Y1 /= 21) OR (Y2 /= 2) OR (Y3 /= 3) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 2");
     END IF;

     T.E (O1=>Y1, O3=>Y3, I1=>31, I3=>33, O2=>Y2);
     IF (Y1 /= 31) OR (Y2 /= 2) OR (Y3 /= 33) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 3");
     END IF;

     T.E (41, 42, O1=>Y1, O2=>Y2, O3=>Y3);
     IF (Y1 /= 41) OR (Y2 /= 42) OR (Y3 /= 3) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 4");
     END IF;

     T.E (51, O3=>Y3, O1=>Y1, O2=>Y2, I3=>53);
     IF (Y1 /= 51) OR (Y2 /= 2) OR (Y3 /= 53) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 5");
     END IF;

     RESULT;

END C95082G;
