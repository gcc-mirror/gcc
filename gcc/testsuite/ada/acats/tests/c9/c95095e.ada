-- C95095E.ADA

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

--     (E) A SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE PART,
--         AN ENTRY IN A TASK, AND ONE HAS ONE MORE PARAMETER
--         THAN THE OTHER; THE OMITTED PARAMETER HAS A DEFAULT VALUE.

-- JWC 7/30/85
-- JRK 10/2/85

WITH REPORT; USE REPORT;
PROCEDURE C95095E IS

BEGIN
     TEST ("C95095E", "SUBPROGRAM/ENTRY OVERLOADING WITH " &
                      "MINIMAL DIFFERENCES ALLOWED");

     --------------------------------------------------

     -- A SUBPROGRAM IS IN AN OUTER DECLARATIVE
     -- PART, AN ENTRY IN A TASK, AND ONE
     -- HAS ONE MORE PARAMETER (WITH A DEFAULT
     -- VALUE) THAN THE OTHER.

     DECLARE
          S : STRING (1..3) := "123";

          PROCEDURE E (I1, I2, I3 : INTEGER := 1) IS
               C : CONSTANT STRING := "CXA";
          BEGIN
               S (I3) := C (I3);
          END E;

          TASK T IS
               ENTRY E (I1, I2 : INTEGER := 1);
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT E (I1, I2 : INTEGER := 1) DO
                    S (2) := 'B';
               END E;
          END T;

     BEGIN

          E (1, 2, 3);
          T.E (1, 2);
          E (1, 2);

          IF S /= "CBA" THEN
               FAILED ("PROCEDURES/ENTRIES DIFFERING " &
                       "ONLY IN EXISTENCE OF ONE " &
                       "DEFAULT-VALUED PARAMETER CAUSED " &
                       "CONFUSION");
          END IF;

     END;

     --------------------------------------------------

     RESULT;
END C95095E;
