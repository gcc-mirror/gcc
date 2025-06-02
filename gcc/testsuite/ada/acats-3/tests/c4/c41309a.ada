-- C41309A.ADA

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
-- CHECK THAT AN EXPANDED NAME IS ALLOWED EVEN IF A USE CLAUSE MAKES THE
-- EXPANDED NAME UNNECESSARY.

-- TBN  12/15/86

WITH REPORT; USE REPORT;
PROCEDURE C41309A IS

BEGIN
     TEST ("C41309A", "CHECK THAT AN EXPANDED NAME IS ALLOWED EVEN " &
                      "IF A USE CLAUSE MAKES THE EXPANDED NAME " &
                      "UNNECESSARY");
     DECLARE
          PACKAGE P IS
               PACKAGE Q IS
                    PACKAGE R IS
                         TYPE REC IS
                              RECORD
                                   A : INTEGER := 5;
                                   B : BOOLEAN := TRUE;
                              END RECORD;
                         REC1 : REC;
                    END R;

                    USE R;

                    REC2 : R.REC := R.REC1;
               END Q;

               USE Q; USE R;

               REC3 : Q.R.REC := Q.REC2;
          END P;

          USE P; USE Q; USE R;

          REC4 : P.Q.R.REC := P.REC3;
     BEGIN
          IF REC4 /= (IDENT_INT(5), IDENT_BOOL(TRUE)) THEN
               FAILED ("INCORRECT RESULTS FROM EXPANDED NAME");
          END IF;
     END;

     RESULT;
END C41309A;
