-- A74105B.ADA

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
-- CHECK THAT THE FULL TYPE DECLARATION OF A PRIVATE TYPE WITHOUT
-- DISCRIMINANTS MAY BE A CONSTRAINED TYPE WITH DISCRIMINANTS.

-- DSJ 4/29/83
-- SPS 10/22/83

WITH REPORT;
PROCEDURE A74105B IS

     USE REPORT;

BEGIN

     TEST ("A74105B", "CHECK THAT THE FULL TYPE DECLARATION OF A " &
                      "PRIVATE TYPE WITHOUT DISCRIMINANTS MAY BE " &
                      "A CONSTRAINED TYPE WITH DISCRIMINANTS");

     DECLARE

          TYPE REC1 (D : INTEGER) IS
               RECORD
                    C1, C2 : INTEGER;
               END RECORD;

          TYPE REC2 (F : INTEGER := 0) IS
               RECORD
                    E1, E2 : INTEGER;
               END RECORD;

          TYPE REC3 IS NEW REC1 (D => 1);

          TYPE REC4 IS NEW REC2 (F => 2);

          PACKAGE PACK1 IS
               TYPE P1 IS PRIVATE;
               TYPE P2 IS PRIVATE;
               TYPE P3 IS PRIVATE;
               TYPE P4 IS PRIVATE;
          PRIVATE
               TYPE P1 IS ACCESS REC1;
               TYPE P2 IS NEW REC4;
               TYPE P3 IS NEW REC1 (D => 5);
               TYPE P4 IS NEW REC2 (F => 7);
          END PACK1;

     BEGIN

          NULL;

     END;

     RESULT;

END A74105B;
