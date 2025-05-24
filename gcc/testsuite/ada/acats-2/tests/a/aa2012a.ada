-- AA2012A.ADA

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
-- CHECK THAT A BODY STUB CAN SERVE AS AN IMPLICIT DECLARATION OF A 
-- SUBPROGRAM, I.E., A PRECEDING SUBPROGRAM DECLARATION IS NOT 
-- REQUIRED.

-- R.WILLIAMS 9/18/86

PROCEDURE AA2012A1 IS

     I : INTEGER;

     PROCEDURE AA2012A2 IS SEPARATE;

     FUNCTION AA2012A3 RETURN INTEGER IS SEPARATE;

BEGIN
     AA2012A2;
     I := AA2012A3;

END AA2012A1;

SEPARATE (AA2012A1)
PROCEDURE AA2012A2 IS
BEGIN
     NULL;
END;

SEPARATE (AA2012A1)
FUNCTION AA2012A3 RETURN INTEGER IS
BEGIN
     RETURN 5;
END;
     
WITH AA2012A1;
WITH REPORT; USE REPORT;
PROCEDURE AA2012A IS

BEGIN
     TEST ( "AA2012A", "CHECK THAT A BODY STUB CAN SERVE AS AN " &
                       "IMPLICIT DECLARATION OF A SUBPROGRAM, " &
                       "I.E., A PRECEDING SUBPROGRAM DECLARATION " &
                       "IS NOT REQUIRED" );

     AA2012A1;

     RESULT;
END AA2012A;
