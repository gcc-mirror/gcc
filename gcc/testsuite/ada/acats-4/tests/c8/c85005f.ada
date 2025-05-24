-- C85005F.ADA

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
--     CHECK THAT, FOR A RENAMED VARIABLE DESIGNATED BY AN ACCESS VALUE,
--     A CHANGE IN THE ACCESS VALUE DOES NOT AFFECT WHICH VARIABLE IS
--     DENOTED BY THE NEW NAME.

-- HISTORY:
--     JET 07/26/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85005F IS
     TYPE ACC IS ACCESS INTEGER;

     BUMP : INTEGER := 0;

     A : ACC := NULL;

     FUNCTION GET_POINTER RETURN ACC IS
     BEGIN
          BUMP := IDENT_INT(BUMP) + 1;
          RETURN NEW INTEGER'(BUMP);
     END GET_POINTER;

BEGIN
     TEST ("C85005F", "CHECK THAT, FOR A RENAMED VARIABLE DESIGNATED " &
                      "BY AN ACCESS VALUE, A CHANGE IN THE ACCESS " &
                      "VALUE DOES NOT AFFECT WHICH VARIABLE IS " &
                      "DENOTED BY THE NEW NAME");

     A := GET_POINTER;

     DECLARE
          X1 : INTEGER RENAMES A.ALL;
          X2 : INTEGER RENAMES GET_POINTER.ALL;
     BEGIN
          A := GET_POINTER;

          IF X1 /= 1 THEN
               FAILED("CHANGING ACCESS VALUE CHANGED RENAMED VARIABLE");
          END IF;

          IF X2 /= 2 THEN
               FAILED("INCORRECT RESULT FROM FUNCTION AS PREFIX");
          END IF;
     END;

     RESULT;
END C85005F;
