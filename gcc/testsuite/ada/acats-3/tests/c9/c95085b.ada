-- C95085B.ADA

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
-- WITH RESPECT TO PARAMETERS OF RECORD TYPES IN ENTRY CALLS.  SUBTESTS
-- INVOLVE ACTUAL RECORD PARAMETERS WHOSE CONSTRAINT VALUES ARE NOT
-- EQUAL TO THE CONSTRAINTS ON THEIR CORRESPONDING FORMAL PARAMETERS:
--        (A) IN PARAMETER, STATIC AGGREGATE.
--        (B) IN PARAMETER, DYNAMIC AGGREGATE.
--        (C) IN PARAMETER, VARIABLE.
--        (D) IN OUT PARAMETER, EXCEPTION RAISED ON CALL.
--        (E) OUT PARAMETER, EXCEPTION RAISED ON CALL.

-- JWC 10/25/85

WITH REPORT; USE REPORT;
PROCEDURE C95085B IS

     SUBTYPE INT IS INTEGER RANGE 0..10;

     TYPE REC (N : INT := 0) IS
          RECORD
               A : STRING (1..N);
          END RECORD;

     SUBTYPE SREC IS REC(N=>3);

BEGIN

     TEST ("C95085B", "CHECK RAISING OF CONSTRAINT_ERROR FOR " &
                      "PARAMETERS OF RECORD TYPES");

     DECLARE

          TASK TSK1 IS
               ENTRY E (R : IN SREC);
          END TSK1;

          TASK BODY TSK1 IS
          BEGIN
               LOOP
                    BEGIN
                         SELECT
                              ACCEPT E (R : IN SREC) DO
                                   FAILED ("EXCEPTION NOT RAISED ON " &
                                           "CALL TO TSK1");
                              END E;
                         OR
                              TERMINATE;
                         END SELECT;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ("EXCEPTION RAISED IN TSK1");
                    END;
               END LOOP;
          END TSK1;

     BEGIN

          BEGIN -- (A)
               TSK1.E ((2,"AA"));
               FAILED ("EXCEPTION NOT RAISED IN SUBTEST (A)");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (A)");
          END; -- (A)

          BEGIN -- (B)
               TSK1.E ((IDENT_INT(2), "AA"));
               FAILED ("EXCEPTION NOT RAISED IN SUBTEST (B)");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (B)");
          END; -- (B)

          DECLARE -- (C)
               R : REC     := (IDENT_INT(2), "AA");
          BEGIN -- (C)
               TSK1.E (R);
               FAILED ("EXCEPTION NOT RAISED IN SUBTEST (C)");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (C)");
          END; -- (C)

     END;

     DECLARE -- (D)

          R : REC := (IDENT_INT(2), "AA");

          TASK TSK2 IS
               ENTRY E (R : IN OUT SREC);
          END TSK2;

          TASK BODY TSK2 IS
          BEGIN
               SELECT
                    ACCEPT E (R : IN OUT SREC) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL TO " &
                                 "TSK2");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TSK2");
          END TSK2;

     BEGIN -- (D)
          TSK2.E (R);
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (D)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (D)");
     END; -- (D)

     DECLARE -- (E)

          R : REC;

          TASK TSK3 IS
               ENTRY E (R : OUT SREC);
          END TSK3;

          TASK BODY TSK3 IS
          BEGIN
               SELECT
                    ACCEPT E (R : OUT SREC) DO
                         FAILED ("EXCEPTION NOT RAISED ON CALL TO " &
                                 "TSK3");
                    END E;
               OR
                    TERMINATE;
               END SELECT;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED IN TSK3");
          END TSK3;

     BEGIN -- (E)
          TSK3.E (R);
          FAILED ("EXCEPTION NOT RAISED IN SUBTEST (E)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
                FAILED ("WRONG EXCEPTION RAISED IN SUBTEST (E)");
     END; -- (E)

     RESULT;

END C95085B;
