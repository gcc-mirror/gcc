-- C92002A.ADA

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
-- CHECK THAT ASSIGNMENT TO A COMPONENT (FOR WHICH ASSIGNMENT IS
--   AVAILABLE) OF A RECORD CONTAINING A TASK IS AVAILABLE.

-- JRK 9/17/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT; USE REPORT;
PROCEDURE C92002A IS

BEGIN
     TEST ("C92002A", "CHECK THAT CAN ASSIGN TO ASSIGNABLE " &
                      "COMPONENTS OF RECORDS WITH TASK " &
                      "COMPONENTS");

     DECLARE

          TASK TYPE TT IS
               ENTRY E;
          END TT;

          TYPE RT IS
               RECORD
                    I : INTEGER := 0;
                    T : TT;
                    J : INTEGER := 0;
               END RECORD;

          R : RT;

          TASK BODY TT IS
          BEGIN
               NULL;
          END TT;

     BEGIN

          R.I := IDENT_INT (7);
          R.J := IDENT_INT (9);

          IF R.I /= 7 AND R.J /= 9 THEN
               FAILED ("WRONG VALUE(S) WHEN ASSIGNING TO " &
                       "INTEGER COMPONENTS OF RECORDS WITH " &
                       "TASK COMPONENTS");
          END IF;

     END;

     RESULT;
END C92002A;
