-- AD8011A.TST

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
--     CHECK THAT CODE STATEMENTS ARE ALLOWED IN A PROCEDURE BODY.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS THAT SUPPORT
--     MACHINE CODE INSERTIONS.

--     IF SUCH INSERTIONS ARE NOT SUPPORTED, THE "WITH MACHINE_CODE"
--     CLAUSE MUST BE REJECTED.


-- MACRO SUBSTITUTION:
--     IF MACHINE CODE INSERTIONS ARE SUPPORTED THEN THE MACRO
--     $MACHINE_CODE_STATEMENT MUST BE REPLACED BY A VALID CODE
--     STATEMENT.

--     IF MACHINE CODE INSERTIONS ARE NOT SUPPORTED, THEN SUBSTITUTE
--     THE ADA NULL STATEMENT (IE: NULL;) FOR $MACHINE_CODE_STATEMENT.

-- HISTORY:
--     DHH 08/30/88  CREATED ORIGINAL TEST.

WITH MACHINE_CODE;                            -- N/A => ERROR.
USE MACHINE_CODE;
WITH REPORT; USE REPORT;
PROCEDURE AD8011A IS

     PROCEDURE CODE IS
     BEGIN
          $MACHINE_CODE_STATEMENT
     END;

BEGIN
     TEST("AD8011A", "CHECK THAT CODE STATEMENTS ARE ALLOWED IN " &
                     "A PROCEDURE BODY");

     CODE;

     RESULT;
END AD8011A;
