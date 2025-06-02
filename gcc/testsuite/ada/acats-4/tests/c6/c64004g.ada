-- C64004G.ADA

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
-- CHECK THAT FOR CALLS TO SUBPROGRAMS HAVING AT LEAST ONE DEFAULT
--   PARAMETER, THE CORRECT ASSOCIATION IS MADE BETWEEN ACTUAL AND
--   FORMAL PARAMETERS.

-- DAS  1/27/81


WITH REPORT;
PROCEDURE C64004G IS

     USE REPORT;

     Y1,Y2,Y3  : INTEGER := 0;
     O1,O2     : INTEGER := 0;

     PROCEDURE P (I1: INTEGER; I2: INTEGER := 2; I3: INTEGER := 3;
                  O1,O2,O3: OUT INTEGER) IS
     BEGIN
          O1 := I1;
          O2 := I2;
          O3 := I3;
     END P;

     FUNCTION F (I1: INTEGER := 1; I2: INTEGER) RETURN INTEGER IS
     BEGIN
          C64004G.O1 := I1;
          C64004G.O2 := I2;
          RETURN 1;
     END F;

BEGIN

     TEST ("C64004G", "CHECK ASSOCIATIONS BETWEEN ACTUAL AND FORMAL" &
                      " PARAMETERS (HAVING DEFAULT VALUES)");

     P (I1=>11, I2=>12, I3=>13, O1=>Y1, O2=>Y2, O3=>Y3);
     IF (Y1 /= 11) OR (Y2 /= 12) OR (Y3 /= 13) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 1");
     END IF;

     P (I1=>21, O1=>Y1, O2=>Y2, O3=>Y3);
     IF (Y1 /= 21) OR (Y2 /= 2) OR (Y3 /= 3) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 2");
     END IF;

     P (O1=>Y1, O3=>Y3, I1=>31, I3=>33, O2=>Y2);
     IF (Y1 /= 31) OR (Y2 /= 2) OR (Y3 /= 33) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 3");
     END IF;

     P (41, 42, O1=>Y1, O2=>Y2, O3=>Y3);
     IF (Y1 /= 41) OR (Y2 /= 42) OR (Y3 /= 3) THEN
          FAILED ("INCORRECT PARANETER ASSOCIATION - 4");
     END IF;

     P (51, O3=>Y3, O1=>Y1, O2=>Y2, I3=>53);
     IF (Y1 /= 51) OR (Y2 /= 2) OR (Y3 /= 53) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 5");
     END IF;

     Y1 := F (I1=>61, I2=>62);
     IF (O1 /= 61) OR (O2 /= 62) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 6");
     END IF;

     Y2 := F (I2=>72, I1=>71);
     IF (O1 /= 71) OR (O2 /= 72) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 7");
     END IF;

     Y3 := F (I2=>82);
     IF (O1 /= 1) OR (O2 /= 82) THEN
          FAILED ("INCORRECT PARAMETER ASSOCIATION - 8");
     END IF;

     RESULT;

END C64004G;
