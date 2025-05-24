-- C95088A.ADA

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
-- CHECK THAT ACTUAL PARAMETERS ARE EVALUATED AND IDENTIFIED AT THE
--   TIME OF CALL.

-- GLH 7/10/85

WITH REPORT; USE REPORT;

PROCEDURE C95088A IS

     TYPE VECTOR IS ARRAY (1..10) OF INTEGER;
     TYPE PTRINT IS ACCESS INTEGER;

     I    : INTEGER := 1;
     A    : VECTOR  := (1,2,3,4,5,6,7,8,9,10);
     P1   : PTRINT  := NEW INTEGER'(2);
     P2   : PTRINT  := P1;

     TASK T1 IS
          ENTRY E1 (I : OUT INTEGER; J : OUT INTEGER);
     END T1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT E1 (I : OUT INTEGER; J : OUT INTEGER) DO
               I := 10;
               J := -1;
          END E1;
     END T1;

     TASK T2 IS
          ENTRY E2 (P : OUT PTRINT; I : OUT INTEGER);
     END T2;

     TASK BODY T2 IS
     BEGIN
          ACCEPT E2 (P : OUT PTRINT; I : OUT INTEGER) DO
               P := NEW INTEGER'(3);
               I := 5;
          END E2;
     END T2;

BEGIN

     TEST ("C95088A", "CHECK THAT ACTUAL PARAMETERS ARE EVALUATED " &
                      "AND IDENTIFIED AT THE TIME OF CALL");

     COMMENT ("FIRST CALL");
     T1.E1 (I, A(I));
     IF (A /= (-1,2,3,4,5,6,7,8,9,10)) THEN
          FAILED ("A(I) EVALUATED UPON RETURN");
     END IF;

     COMMENT ("SECOND CALL");
     T2.E2 (P1, P1.ALL);
     IF (P2.ALL /= 5) THEN
          FAILED ("P1.ALL EVALUATED UPON RETURN");
     END IF;

     RESULT;

END C95088A;
