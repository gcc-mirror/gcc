-- C94006A.ADA

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
-- CHECK THAT A DECLARATION THAT RENAMES A TASK DOES NOT CREATE A NEW
-- MASTER FOR THE TASK.

-- TBN  9/17/86
-- PWN 01/31/95  REMOVED PRAGMA PRIORITY FOR ADA 9X.

with Impdef;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE C94006A IS

     TASK TYPE TT IS
          ENTRY E;
     END TT;

     TASK BODY TT IS
     BEGIN
          SELECT
               ACCEPT E;
          OR
               DELAY 30.0 * Impdef.One_Second;
          END SELECT;
     END TT;


BEGIN
     TEST ("C94006A", "CHECK THAT A DECLARATION THAT RENAMES A TASK " &
                      "DOES NOT CREATE A NEW MASTER FOR THE TASK");

     -------------------------------------------------------------------
     DECLARE
          T1 : TT;
     BEGIN
          DECLARE
               RENAME_TASK : TT RENAMES T1;
          BEGIN
               NULL;
          END;
          IF T1'TERMINATED THEN
               FAILED ("TASK DEPENDENT ON WRONG UNIT - 1");
          ELSE
               T1.E;
          END IF;
     END;

     -------------------------------------------------------------------

     DECLARE
          T2 : TT;

          PACKAGE P IS
               Q : TT RENAMES T2;
          END P;

          PACKAGE BODY P IS
          BEGIN
               NULL;
          END P;

          USE P;
     BEGIN
          IF Q'TERMINATED THEN
               FAILED ("TASK DEPENDENT ON WRONG UNIT - 2");
          ELSE
               Q.E;
          END IF;
     END;

     -------------------------------------------------------------------

     DECLARE
          TYPE ACC_TT IS ACCESS TT;
          P1 : ACC_TT;
     BEGIN
          DECLARE
               RENAME_ACCESS : ACC_TT RENAMES P1;
          BEGIN
               RENAME_ACCESS := NEW TT;
          END;
          IF P1'TERMINATED THEN
               FAILED ("TASK DEPENDENT ON WRONG UNIT - 3");
          ELSE
               P1.E;
          END IF;
     END;

     -------------------------------------------------------------------

     DECLARE
          TYPE ACC_TT IS ACCESS TT;
          P2 : ACC_TT;

          PACKAGE Q IS
               RENAME_ACCESS : ACC_TT RENAMES P2;
          END Q;

          PACKAGE BODY Q IS
          BEGIN
               RENAME_ACCESS := NEW TT;
          END Q;

          USE Q;
     BEGIN
          IF RENAME_ACCESS'TERMINATED THEN
               FAILED ("TASK DEPENDENT ON WRONG UNIT - 4");
          ELSE
               RENAME_ACCESS.E;
          END IF;
     END;

     RESULT;
END C94006A;
