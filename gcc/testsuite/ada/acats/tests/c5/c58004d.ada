-- C58004D.ADA

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
-- CHECK THAT A RETURN STATEMENT TERMINATES EXECUTION
--    OF THE INNERMOST ENCLOSING SUBPROGRAM.

-- CHECKS GENERIC SUBPROGRAMS.

-- SPS 3/7/83
-- JRK 1/31/84

WITH REPORT;
PROCEDURE C58004D IS

     USE REPORT;

     I1, I2 : INTEGER;

     GENERIC
     PROCEDURE ADDM (IA1 : IN OUT INTEGER; IA2 : IN INTEGER);

     PROCEDURE ADDM (IA1 : IN OUT INTEGER; IA2 : IN INTEGER) IS

          GENERIC
          PROCEDURE MULT (IM1 : IN OUT INTEGER; IM2 : IN INTEGER);

          PROCEDURE MULT (IM1 : IN OUT INTEGER; IM2 : IN INTEGER) IS
          BEGIN
               IM1 := IM1 * IM2;

               IF IM1 > 0 THEN RETURN;
               END IF;

               IM1 := 0;
          END MULT;

          PROCEDURE MLT IS NEW MULT;

     BEGIN
          MLT (IA1, IA2);
          IA1 := IA1 + IA2;

          IF IA1 > 0 THEN RETURN;
          END IF;

          IA1 := 0;
     END ADDM;

     PROCEDURE ADM IS NEW ADDM;

BEGIN
     TEST ("C58004D","CHECK THAT RETURN TERMINATES EXECUTION OF ONLY" &
           " THE INNERMOST ENCLOSING GENERIC SUBPROGRAM");

     I1 := 2;
     I2 := 3;
     ADM (I1,I2);     -- SAME AS I1 := (I1 * I2) + I2

     IF I1 = 0 THEN
          FAILED ("RETURN DOES NOT TERMINATE SUBPROGRAM");
     ELSIF I1 = 6 THEN
          FAILED
          ("RETURN TERMINATES ALL SUBPROGRAMS NOT JUST INNERMOST");
     ELSIF I1 /= 9 THEN
          FAILED ("RETURN STATEMENT NOT WORKING CORRECTLY");
     END IF;

     RESULT;
END C58004D;
