-- C95010A.ADA

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
-- CHECK THAT A TASK MAY CONTAIN MORE THAN ONE ACCEPT_STATEMENT
--   FOR AN ENTRY.

-- THIS TEST CONTAINS SHARED VARIABLES.

-- JRK 11/5/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT; USE REPORT;
PROCEDURE C95010A IS

     V : INTEGER := 0;

BEGIN
     TEST ("C95010A", "CHECK THAT A TASK MAY CONTAIN MORE THAN " &
                      "ONE ACCEPT_STATEMENT FOR AN ENTRY");

     DECLARE

          SUBTYPE INT IS INTEGER RANGE 1..5;

          TASK T IS
               ENTRY E;
               ENTRY EF (INT) (I : INTEGER);
          END T;

          TASK BODY T IS
          BEGIN
               V := 1;
               ACCEPT E;
               V := 2;
               ACCEPT E;
               V := 3;
               ACCEPT EF (2) (I : INTEGER) DO
                    V := I;
               END EF;
               V := 5;
               ACCEPT EF (2) (I : INTEGER) DO
                    V := I;
               END EF;
               V := 7;
          END T;

     BEGIN

          T.E;
          T.E;
          T.EF (2) (4);
          T.EF (2) (6);

     END;

     IF V /= 7 THEN
          FAILED ("WRONG CONTROL FLOW VALUE");
     END IF;

     RESULT;
END C95010A;
