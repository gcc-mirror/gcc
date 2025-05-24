-- C95076A.ADA

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
-- CHECK THAT AN ACCEPT STATEMENT WITH AND WITHOUT A RETURN
-- STATEMENT RETURNS CORRECTLY.

-- GLH  7/11/85

WITH REPORT; USE REPORT;

PROCEDURE C95076A IS

     I : INTEGER;

     TASK T1 IS
          ENTRY E1 (N : IN OUT INTEGER);
     END T1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT E1 (N : IN OUT INTEGER) DO
               IF (N = 5) THEN
                    N := N + 5;
               ELSE
                    N := 0;
               END IF;
          END E1;
     END T1;

     TASK T2 IS
          ENTRY E2 (N : IN OUT INTEGER);
     END T2;

     TASK BODY T2 IS
     BEGIN
          ACCEPT E2 (N : IN OUT INTEGER) DO
               IF (N = 10) THEN
                    N := N + 5;
                    RETURN;
               END IF;
               N := 0;
          END E2;
     END T2;

BEGIN

     TEST ("C95076A", "CHECK THAT AN ACCEPT STATEMENT WITH AND " &
                      "WITHOUT A RETURN STATEMENT RETURNS CORRECTLY");

     I := 5;
     T1.E1 (I);
     IF (I /= 10) THEN
          FAILED ("INCORRECT RENDEVOUS WITHOUT A RETURN");
     END IF;

     I := 10;
     T2.E2 (I);
     IF (I /= 15) THEN
          FAILED ("INCORRECT RENDEVOUS WITH A RETURN");
     END IF;

     RESULT;

END C95076A;
