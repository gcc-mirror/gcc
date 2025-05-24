-- C45536A.DEP

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
--     CHECK FIXED POINT MULTIPLICATION AND DIVISION WHEN 'SMALL OF
--     THE OPERANDS ARE NOT BOTH POWERS OF THE SAME BASE VALUE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     REPRESENTATION CLAUSES FOR 'SMALL WHICH ARE NOT POWERS OF TWO.

--     IF SUCH REPRESENTATION CLAUSES ARE NOT SUPPORTED, THEN THE
--     REPRESENTATION CLAUSE FOR CHECK_TYPE MUST BE REJECTED.

-- HISTORY:
--     BCB 02/02/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C45536A IS

     TYPE CHECK_TYPE IS DELTA 2.0**(-1) RANGE 0.0 .. 8.0;
     FOR CHECK_TYPE'SMALL USE 0.2;                     -- N/A => ERROR.

     TYPE F1 IS DELTA 2.0**(-1) RANGE 0.0 .. 8.0;
     FOR F1'SMALL USE 0.5;

     TYPE F2 IS DELTA 2.0**(-1) RANGE 0.0 .. 8.0;
     FOR F2'SMALL USE 0.2;

     TYPE F3 IS DELTA 2.0**(-1) RANGE 0.0 .. 8.0;
     FOR F3'SMALL USE 0.1;

     A : F1;
     B : F2;
     C : F3;

     FUNCTION IDENT_FIX(X : F3) RETURN F3 IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN X;
          ELSE
               RETURN 0.0;
          END IF;
     END IDENT_FIX;

BEGIN
     TEST ("C45536A", "CHECK FIXED POINT MULTIPLICATION AND DIVISION " &
                      "WHEN 'SMALL OF THE OPERANDS ARE NOT BOTH " &
                      "POWERS OF THE SAME BASE VALUE");

     A := 1.0; B := 1.0; C := F3(A * B);

     IF C /= IDENT_FIX(1.0) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 1");
     END IF;

     C := F3(A / B);

     IF C /= IDENT_FIX(1.0) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 1");
     END IF;

     A := 1.0; B := 0.3; C := F3(A * B);

     IF C NOT IN IDENT_FIX(0.2) .. IDENT_FIX(0.4) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 2");
     END IF;

     B := 0.25; C := F3(A / B);

     IF C NOT IN IDENT_FIX(2.5) .. IDENT_FIX(5.0) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 2");
     END IF;

     A := 0.5; B := 0.3; C := F3(A * B);

     IF C NOT IN IDENT_FIX(0.1) .. IDENT_FIX(0.2) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 3");
     END IF;

     C := F3(A / B);

     IF C NOT IN IDENT_FIX(1.2) .. IDENT_FIX(2.5) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 3");
     END IF;

     B := 0.3; C := 0.2; A := F1(B * C);

     IF A NOT IN F1(IDENT_FIX(0.0)) .. F1(IDENT_FIX(0.5)) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 4");
     END IF;

     A := 1.0; B := 1.6; C := F3(A / B);

     IF C NOT IN IDENT_FIX(0.6) .. IDENT_FIX(0.7) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 4");
     END IF;

     A := 0.75; B := 0.4; C := F3(A * B);

     IF C NOT IN IDENT_FIX(0.2) .. IDENT_FIX(0.4) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 5");
     END IF;

     A := 0.8; C := F3(A / B);

     IF C NOT IN IDENT_FIX(1.2) .. IDENT_FIX(2.5) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 5");
     END IF;

     A := 0.8; B := 0.4; C := F3(A * B);

     IF C NOT IN IDENT_FIX(0.2) .. IDENT_FIX(0.4) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 6");
     END IF;

     A := 0.75; C := F3(A / B);

     IF C NOT IN IDENT_FIX(1.2) .. IDENT_FIX(2.5) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 6");
     END IF;

     A := 0.7; B := 0.3; C := F3(A * B);

     IF C NOT IN IDENT_FIX(0.1) .. IDENT_FIX(0.4) THEN
          FAILED ("IMPROPER RESULTS FOR MULTIPLICATION - 7");
     END IF;

     C := F3(A / B);

     IF C NOT IN IDENT_FIX(1.2) .. IDENT_FIX(5.0) THEN
          FAILED ("IMPROPER RESULTS FOR DIVISION - 7");
     END IF;

     RESULT;
END C45536A;
