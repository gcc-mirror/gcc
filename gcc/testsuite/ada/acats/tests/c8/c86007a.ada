-- C86007A.ADA

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
--     CHECK THAT AN EXPANDED NAME FOR AN ENTITY DECLARED IN THE VISIBLE
--     PART OF A LIBRARY PACKAGE CAN START WITH THE NAME "STANDARD".

-- HISTORY:
--     DHH 03/15/88 CREATED ORIGINAL TEST.
--     RJW 10/26/89 ADDED "PRAGMA ELABORATE (REPORT);"

WITH REPORT; USE REPORT;
PRAGMA ELABORATE (REPORT);
PACKAGE C86007A_PACK IS
     SUBTYPE ITEM IS INTEGER RANGE 0 .. 10;
     Y : STANDARD.C86007A_PACK.ITEM := IDENT_INT(5);
     TYPE ACC IS ACCESS STANDARD.C86007A_PACK.ITEM;
     PROCEDURE SWAP(X,Y: IN OUT ITEM);
     PROCEDURE PROC;
END C86007A_PACK;

PACKAGE BODY C86007A_PACK IS
     PROCEDURE SWAP(X,Y: IN OUT STANDARD.C86007A_PACK.ITEM) IS
          T : STANDARD.C86007A_PACK.ITEM;
     BEGIN
          T := X;
          X := Y;
          Y := T;
     END SWAP;

     PROCEDURE PROC IS
          X : STANDARD.C86007A_PACK.ITEM := IDENT_INT(10);
          W : STANDARD.C86007A_PACK.ACC;
     BEGIN

          W := NEW STANDARD.C86007A_PACK.ITEM;
          W.ALL := X;
          STANDARD.C86007A_PACK.SWAP(X, STANDARD.C86007A_PACK.Y);
          IF STANDARD.C86007A_PACK.Y /= IDENT_INT(10) THEN
               FAILED("FAILED STANDARD.NAME CALL PROCEDURE - B-10");
          END IF;
          IF X /= IDENT_INT(5) THEN
               FAILED("FAILED STANDARD.NAME CALL PROCEDURE - B-5");
          END IF;
     END PROC;
END C86007A_PACK;

WITH C86007A_PACK; WITH REPORT; USE REPORT;
PROCEDURE C86007A IS
BEGIN
     TEST("C86007A", "CHECK THAT AN EXPANDED NAME FOR AN ENTITY " &
                     "DECLARED IN THE VISIBLE PART OF A LIBRARY " &
                     "PACKAGE CAN START WITH THE NAME ""STANDARD""");

     STANDARD.C86007A_PACK.PROC;

     RESULT;
END C86007A;
