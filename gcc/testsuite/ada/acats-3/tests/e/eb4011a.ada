-- EB4011A.ADA

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
--     CHECK THAT UNHANDLED EXCEPTIONS RAISED IN PACKAGE SUBUNITS ARE
--     PROPAGATED TO THE ENVIRONMENT STATICALLY ENCLOSING THE
--     CORRESPONDING BODY STUB (DECLARER OF THE PARENT UNIT).

-- PASS/FAIL CRITERIA:
--     THIS TEST MUST EXECUTE AND REPORT "TENTATIVELY PASSED". IN
--     ADDITION, THE OUTPUT/LOG FILE MUST INDICATE THAT THE PROGRAM
--     TERMINATED WITH AN UNHANDLED EXCEPTION.

-- HISTORY:
--     DHH 03/29/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE EB4011A IS

     PACKAGE EB4011A_OUTSIDE IS
     END EB4011A_OUTSIDE;

     PACKAGE EB4011A1 IS
     END EB4011A1;

     PACKAGE BODY EB4011A1 IS
     BEGIN

          TEST("EB4011A", "CHECK THAT UNHANDLED EXCEPTIONS RAISED IN " &
                          "PACKAGE SUBUNITS ARE PROPAGATED TO THE " &
                          "ENVIRONMENT STATICALLY ENCLOSING THE" &
                          "CORRESPONDING BODY STUB (DECLARER OF THE " &
                          "PARENT UNIT)");

          SPECIAL_ACTION("CHECK THE OUTPUT FILE TO SEE IF THIS " &
                         "PROGRAM TERMINATED WITH AN UNHANDLED " &
                         "EXCEPTION");

          RESULT;

     END EB4011A1;

     PACKAGE BODY EB4011A_OUTSIDE IS SEPARATE;

BEGIN

     TEST("EB4011A", "THIS LINE SHOULD NOT PRINT OUT");

     FAILED("EXCEPTION DID NOT CAUSE MAIN PROGRAM TERMINATION");
     RESULT;

END EB4011A;

SEPARATE (EB4011A)
PACKAGE BODY EB4011A_OUTSIDE IS
BEGIN
     RAISE CONSTRAINT_ERROR;
END EB4011A_OUTSIDE;
