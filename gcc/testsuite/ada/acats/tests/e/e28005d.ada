PRAGMA PAGE;
-- E28005D.ADA

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
--     CHECK THAT WHEN PRAGMA PAGE IS USED AT THE BEGINNING OR END OF A
--     COMPILATION, THERE IS NO PROBLEM.

-- PASS/FAIL CRITERIA:
--     THE TEST MUST COMPILE TO EXECUTE WITH A 'TENTATIVELY PASSED'
--     RESULT.  THERE IS A PAGE BREAK BEFORE THE TEST NAME AND A
--     PAGE BREAK AFTER THE END OF THE TEST.

-- HISTORY:
--     RJW 04/16/86  CREATED ORIGINAL TEST.
--     JET 01/13/88  ADDED CALLS TO SPECIAL_ACTION AND UPDATED HEADER.

WITH REPORT; USE REPORT;

PROCEDURE E28005D IS
BEGIN
     TEST ( "E28005D", "CHECK THAT WHEN PRAGMA PAGE IS USED AT THE " &
                       "BEGINNING OR END OF A COMPILATION, THERE " &
                       "IS NO PROBLEM");

     SPECIAL_ACTION ("CHECK THAT THE PAGE PRAGMAS AT THE BEGINNING " &
                     "AND END OF THE PROGRAM CAUSE THE TEXT " &
                     "FOLLOWING THE PRAGMAS TO APPEAR AT THE START " &
                     "OF A NEW PAGE OF THE COMPILATION LISTING");
     RESULT;

END E28005D;

PRAGMA PAGE;
