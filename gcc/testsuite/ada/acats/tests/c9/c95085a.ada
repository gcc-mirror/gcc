-- C95085A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR OUT OF RANGE SCALAR
-- ARGUMENTS.  SUBTESTS ARE:
--        (A) STATIC IN ARGUMENT.
--        (B) DYNAMIC IN ARGUMENT.
--        (C) IN OUT, OUT OF RANGE ON CALL.
--        (D) OUT, OUT OF RANGE ON RETURN.
--        (E) IN OUT, OUT OF RANGE ON RETURN.

-- GLH 7/15/85
-- JRK 8/23/85
-- JWC 11/15/85     ADDED VARIABLE "CALLED" TO ENSURE THAT THE ENTRY
--                  CALL WAS MADE FOR THOSE CASES THAT ARE APPLICABLE.

WITH REPORT;  USE REPORT;
PROCEDURE C95085A IS

     SUBTYPE DIGIT IS INTEGER RANGE 0..9;

     D      : DIGIT;
     I      : INTEGER;
     M1     : CONSTANT INTEGER := IDENT_INT (-1);
     COUNT  : INTEGER := 0;
     CALLED : BOOLEAN;

     SUBTYPE SI IS INTEGER RANGE M1 .. 10;

     TASK T1 IS
          ENTRY E1 (PIN : IN DIGIT; WHO : STRING);  -- (A), (B).
     END T1;

     TASK BODY T1 IS
     BEGIN
          LOOP
               BEGIN
                    SELECT
                         ACCEPT E1 (PIN : IN DIGIT;
                                    WHO : STRING) DO  -- (A), (B).
                              FAILED ("EXCEPTION NOT RAISED BEFORE " &
                                      "CALL - E1 " & WHO);
                         END E1;
                    OR
                         TERMINATE;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN E1");
               END;
          END LOOP;
     END T1;

     TASK T2 IS
          ENTRY E2 (PINOUT : IN OUT DIGIT; WHO : STRING);  -- (C).
     END T2;

     TASK BODY T2 IS
     BEGIN
          LOOP
               BEGIN
                    SELECT
                         ACCEPT E2 (PINOUT : IN OUT DIGIT;
                                    WHO : STRING) DO  -- (C).
                              FAILED ("EXCEPTION NOT RAISED BEFORE " &
                                      "CALL - E2 " & WHO);
                         END E2;
                    OR
                         TERMINATE;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN E2");
               END;
          END LOOP;
     END T2;

     TASK T3 IS
          ENTRY E3 (POUT : OUT SI; WHO : STRING);  -- (D).
     END T3;

     TASK BODY T3 IS
     BEGIN
          LOOP
               BEGIN
                    SELECT
                         ACCEPT E3 (POUT : OUT SI;
                                    WHO : STRING) DO  -- (D).
                              CALLED := TRUE;
                              IF WHO = "10" THEN
                                   POUT := IDENT_INT (10);  -- 10 IS NOT
                                                            -- A DIGIT.
                              ELSE
                                   POUT := -1;
                              END IF;
                         END E3;
                    OR
                         TERMINATE;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN E3");
               END;
          END LOOP;
     END T3;

     TASK T4 IS
          ENTRY E4 (PINOUT : IN OUT INTEGER; WHO : STRING);  -- (E).
     END T4;

     TASK BODY T4 IS
     BEGIN
          LOOP
               BEGIN
                    SELECT
                         ACCEPT E4 (PINOUT : IN OUT INTEGER;
                                    WHO : STRING) DO  -- (E).
                              CALLED := TRUE;
                              IF WHO = "10" THEN
                                   PINOUT := 10;  -- 10 IS NOT A DIGIT.
                              ELSE
                                   PINOUT := IDENT_INT (-1);
                              END IF;
                         END E4;
                    OR
                         TERMINATE;
                    END SELECT;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED IN E4");
               END;
          END LOOP;
     END T4;

BEGIN

     TEST ("C95085A", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "FOR OUT OF RANGE SCALAR ARGUMENTS");

     BEGIN  -- (A)
          T1.E1 (10, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR E1 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E1 (10)");
     END;  -- (A)

     BEGIN  -- (B)
          T1.E1 (IDENT_INT (-1), "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR E1 (" &
                  "IDENT_INT (-1))");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E1 (" &
                       "IDENT_INT (-1))");
     END;  -- (B)

     BEGIN  -- (C)
          I := IDENT_INT (10);
          T2.E2 (I, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR E2 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E2 (10)");
     END;  -- (C)

     BEGIN -- (C1)
          I := IDENT_INT (-1);
          T2.E2 (I, "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR E2 (-1)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E2 (-1)");
     END; -- (C1)

     BEGIN  -- (D)
          CALLED := FALSE;
          D := IDENT_INT (1);
          T3.E3 (D, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " &
                  "E3 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL " &
                            "E3 (10)");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E3 (10)");
     END;  -- (D)

     BEGIN -- (D1)
          CALLED := FALSE;
          D := IDENT_INT (1);
          T3.E3 (D, "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " &
                  "E3 (-1)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL " &
                            "E3 (-1)");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E3 (-1)");
     END; -- (D1)

     BEGIN  -- (E)
          CALLED := FALSE;
          D := 9;
          T4.E4 (D, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " &
                  "E4 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL " &
                            "E4 (10)");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E4 (10)");
     END;  -- (E)

     BEGIN -- (E1)
          CALLED := FALSE;
          D := 0;
          T4.E4 (D, "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM " &
                  "E4 (-1)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("EXCEPTION RAISED BEFORE CALL " &
                            "E4 (-1)");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR E4 (-1)");
     END; -- (E1)

     IF COUNT /= 8 THEN
          FAILED ("INCORRECT NUMBER OF CONSTRAINT_ERRORS RAISED");
     END IF;

     RESULT;

END C95085A;
