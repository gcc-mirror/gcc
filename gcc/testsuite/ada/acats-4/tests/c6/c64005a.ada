-- C64005A.ADA

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
-- CHECK THAT A SUBPROGRAM CAN BE CALLED
-- RECURSIVELY AND THAT NON-LOCAL VARIABLES AND 
-- CONSTANTS ARE PROPERLY ACCESSED FROM WITHIN
-- RECURSIVE INVOCATIONS.

-- CVP 5/1/81

WITH REPORT;
PROCEDURE C64005A IS

     USE REPORT;

     TWENTY : CONSTANT INTEGER := 20;
     C1 : CONSTANT INTEGER := 1;
     I1, I2 : INTEGER := 0;

     PROCEDURE RECURSE (I1A : INTEGER; I2 : IN OUT INTEGER) IS
          C1 : CONSTANT INTEGER := 5;
     BEGIN
          IF I1A < TWENTY THEN
               RECURSE (I1A+C1, I2);
               I1 := I1 + C64005A.C1;
               I2 := I2 + I1A;
          END IF;
     END RECURSE;

BEGIN
     TEST ("C64005A", "RECURSIVE SUBPROGRAMS WITH " &
           "NON-LOCAL DATA ACCESS");

     RECURSE (0, I2);

     IF I1 /= 4 OR I2 /= 30 THEN
          FAILED ("RECURSIVE PROCEDURE INVOCATIONS " &
                  "WITH GLOBAL DATA ACCESS NOT PERFORMED " &
                  "CORRECTLY");
     END IF;

     RESULT;
END C64005A;
