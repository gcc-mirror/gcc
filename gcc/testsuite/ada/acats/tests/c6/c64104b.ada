-- C64104B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED UNDER APPROPRIATE CIRCUMSTANCES
--    WITH RESPECT TO PARAMETERS OF RECORD TYPES.  SUBTESTS INVOLVE 
--    ACTUAL RECORD PARAMETERS WHOSE CONSTRAINT VALUES ARE NOT EQUAL 
--    TO THE CONSTRAINTS ON THEIR CORRESPONDING FORMAL PARAMETERS:
--        (A) IN PARAMETER, STATIC AGGREGATE.
--        (B) IN PARAMETER, DYNAMIC AGGREGATE.
--        (C) IN PARAMETER, VARIABLE.
--        (D) IN OUT PARAMETER, EXCEPTION RAISED ON CALL.
--        (E) OUT PARAMETER, EXCEPTION RAISED ON CALL.

-- DAS 2/11/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C64104B IS

     USE REPORT;
     SUBTYPE INT IS INTEGER RANGE 0..10;
     TYPE REC (N : INT := 0) IS
          RECORD
               A : STRING (1..N);
          END RECORD;
     SUBTYPE SREC IS REC(N=>3);
     PROCEDURE P1 (R : IN SREC) IS
     BEGIN
          FAILED ("EXCEPTION NOT RAISED ON CALL TO P1");
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN PROCEDURE P1");
     END P1;

     PROCEDURE P2 (R : IN OUT SREC) IS
     BEGIN
          FAILED ("EXCEPTION NOT RAISED ON CALL TO P2");
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN PROCEDURE P2");
     END P2;

     PROCEDURE P3 (R : OUT SREC) IS
     BEGIN
          FAILED ("EXCEPTION NOT RAISED ON CALL TO P3");
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN PROCEDURE P3");
     END P3;

BEGIN

     TEST ("C64104B", "CHECK RAISING OF CONSTRAINT_ERROR FOR " &
                      "PARAMETERS OF RECORD TYPES");

     BEGIN     -- (A)
          P1 ((2,"AA"));
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (A)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (A)");
     END; -- (A)

     BEGIN     -- (B)
          P1 ((IDENT_INT(2), "AA"));
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (B)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (B)");
     END; -- (B)

     DECLARE   -- (C)
          R : REC     := (IDENT_INT(2), "AA");
     BEGIN     -- (C)
          P1 (R);
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (C)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (C)");
     END; -- (C)

     DECLARE   -- (D)
          R : REC     := (IDENT_INT(2), "AA");
     BEGIN     -- (D)
          P2 (R);
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (D)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (D)");
     END; -- (D)


     DECLARE   -- (E)
          R  : REC;
     BEGIN     -- (E)
          P3 (R);
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (E)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
                FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (E)");
     END; -- (E)

     RESULT;

END C64104B;
