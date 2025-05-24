-- CD2D13A.ADA

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
--     CHECK THAT A SMALL CLAUSE CAN BE GIVEN IN THE VISIBLE
--     OR PRIVATE PART OF A PACKAGE FOR A FIXED POINT TYPE DECLARED
--     IN THE VISIBLE PART.

-- HISTORY:
--     BCB 09/01/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM; WITH TEXT_IO;
WITH REPORT; USE REPORT;
PROCEDURE CD2D13A IS

     SPECIFIED_SMALL : CONSTANT := 2.0 ** (-4);

     PACKAGE P IS
          TYPE FIXED_IN_P IS DELTA 1.0 RANGE -4.0 .. 4.0;
          FOR FIXED_IN_P'SMALL USE SPECIFIED_SMALL;
          TYPE ALT_FIXED_IN_P IS DELTA 1.0 RANGE -4.0 .. 4.0;
     PRIVATE
          FOR ALT_FIXED_IN_P'SMALL USE SPECIFIED_SMALL;
     END P;

     USE P;

BEGIN

     TEST("CD2D13A", "A SMALL CLAUSE CAN BE GIVEN IN THE VISIBLE " &
                     "OR PRIVATE PART OF A PACKAGE FOR A FIXED " &
                     "POINT TYPE DECLARED IN THE VISIBLE PART");

     IF FIXED_IN_P'SMALL /= SPECIFIED_SMALL THEN
          FAILED ("INCORRECT VALUE FOR FIXED_IN_P'SMALL");
     END IF;

     IF ALT_FIXED_IN_P'SMALL /= SPECIFIED_SMALL THEN
          FAILED ("INCORRECT VALUE FOR ALT_FIXED_IN_P'SMALL");
     END IF;

     RESULT;

END CD2D13A;
