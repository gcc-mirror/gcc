-- CD7202A.ADA

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
--     THE 'ADDRESS ATTRIBUTE CAN BE USED IN A COMPILATION UNIT EVEN IF
--     A WITH CLAUSE FOR PACKAGE SYSTEM DOES NOT APPLY TO THE UNIT.

-- HISTORY:
--     DHH 08/31/88  CREATED ORIGINAL TEST.

WITH SYSTEM;
PACKAGE CD7202A_SYS IS
     SUBTYPE MY_ADDRESS IS SYSTEM.ADDRESS;
END CD7202A_SYS;

WITH CD7202A_SYS;
WITH REPORT; USE REPORT;
PROCEDURE CD7202A IS

     INT : INTEGER := 2;

     BOOL : BOOLEAN := (INT'ADDRESS IN CD7202A_SYS.MY_ADDRESS);

BEGIN
     TEST ("CD7202A", "THE 'ADDRESS ATTRIBUTE CAN BE USED IN A" &
                      " COMPILATION UNIT EVEN IF A WITH CLAUSE FOR " &
                      "PACKAGE SYSTEM DOES NOT APPLY TO THE UNIT");

     IF NOT IDENT_BOOL(BOOL) THEN
          FAILED("ADDRESS ATTRIBUTE INCORRECT");
     END IF;

     RESULT;
END CD7202A;
