-- CD5011C.ADA

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
--     CHECK THAT AN ADDRESS CLAUSE CAN BE GIVEN FOR A VARIABLE OF
--     AN INTEGER TYPE IN THE DECLARATIVE PART OF A PACKAGE BODY.

-- HISTORY:
--     JET 09/11/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
WITH SPPRT13;

PROCEDURE CD5011C IS

     PACKAGE CD5011C_PACKAGE IS
     END CD5011C_PACKAGE;

     PACKAGE BODY CD5011C_PACKAGE IS

          INT : INTEGER := 0;
          FOR INT USE
               AT SPPRT13.VARIABLE_ADDRESS;

     BEGIN
          TEST ("CD5011C", "AN ADDRESS CLAUSE CAN BE " &
                           "GIVEN FOR A VARIABLE OF AN INTEGER " &
                           "TYPE IN THE DECLARATIVE PART OF A " &
                           "PACKAGE BODY");

          IF EQUAL (3, 3) THEN
               INT := 5;
          END IF;
          IF INT /= IDENT_INT (5) THEN
               FAILED ("WRONG VALUE FOR VARIABLE IN PACKAGE");
          END IF;
          IF INT'ADDRESS /= SPPRT13.VARIABLE_ADDRESS THEN
               FAILED ("WRONG ADDRESS FOR VARIABLE IN PACKAGE");
          END IF;
     END;

BEGIN

     RESULT;

END CD5011C;
