-- C64104A.ADA

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
--    CHECK THAT CONSTRAINT_ERROR IS RAISED FOR OUT OF RANGE SCALAR
--      ARGUMENTS.  SUBTESTS ARE:
--           (A) STATIC IN ARGUMENT.
--           (B) DYNAMIC IN ARGUMENT.
--           (C) IN OUT, OUT OF RANGE ON CALL.
--           (D) OUT, OUT OF RANGE ON RETURN.
--           (E) IN OUT, OUT OF RANGE ON RETURN.

-- HISTORY:
--    DAS  01/14/81
--    CPP  07/03/84
--    LB   11/20/86  ADDED CODE TO ENSURE IN SUBTESTS WHICH CHECK
--                     RETURNED VALUES, THAT SUBPROGRAMS ARE ACTUALLY
--                     CALLED.
--    JET  08/04/87  FIXED HEADER FOR STANDARD FORMAT.

WITH REPORT;  USE REPORT;
PROCEDURE C64104A IS

     SUBTYPE DIGIT IS INTEGER RANGE 0..9;

     CALLED : BOOLEAN;
     D    : DIGIT;
     I    : INTEGER;
     M1 : CONSTANT INTEGER := IDENT_INT(-1);
     COUNT     : INTEGER := 0;
     SUBTYPE SI IS INTEGER RANGE M1 .. 10;

     PROCEDURE P1 (PIN : IN DIGIT; WHO : STRING) IS         -- (A), (B)
     BEGIN
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL - P1 " & WHO);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN P1 FOR " & WHO);
     END P1;

     PROCEDURE P2 (PINOUT : IN OUT DIGIT; WHO : STRING) IS  -- (C)
     BEGIN
          FAILED ("EXCEPTION NOT RAISED BEFORE CALL - P2 " & WHO);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN P2 FOR " & WHO);
     END P2;

     PROCEDURE P3 (POUT : OUT SI; WHO : STRING) IS          -- (D)
     BEGIN
          IF WHO = "10" THEN
               POUT := IDENT_INT(10);    -- (10 IS NOT A DIGIT)
          ELSE
               POUT := -1;
          END IF;
          CALLED := TRUE;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN P3 FOR " & WHO);
     END P3;

     PROCEDURE P4 (PINOUT : IN OUT INTEGER; WHO : STRING) IS     -- (E)
     BEGIN
          IF WHO = "10" THEN
               PINOUT := 10;       -- (10 IS NOT A DIGIT)
          ELSE
               PINOUT := IDENT_INT(-1);
          END IF;
          CALLED := TRUE;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED IN P4 FOR" & WHO);
     END P4;

BEGIN

     TEST ("C64104A", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "FOR OUT OF RANGE SCALAR ARGUMENTS");

     BEGIN  -- (A)
          P1 (10, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR P1 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P1 (10)");
     END;  -- (A)

     BEGIN  -- (B)
          P1 (IDENT_INT (-1), "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR P1 (" &
                  "IDENT_INT (-1))");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P1 (" &
                       "IDENT_INT (-1))");
     END;  --(B)

     BEGIN  -- (C)
          I := IDENT_INT (10);
          P2 (I, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR P2 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P2 (10)");
     END;  -- (C)

     BEGIN -- (C1)
          I := IDENT_INT (-1);
          P2 (I, "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED FOR P2 (-1)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P2 (-1)");
     END; -- (C1)

     BEGIN  -- (D)
          CALLED := FALSE;
          D := IDENT_INT (1);
          P3 (D, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" &
                  " P3 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("SUBPROGRAM P3 WAS NOT CALLED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P3 (10)");
     END;  -- (D)

     BEGIN -- (D1)
          CALLED := FALSE;
          D := IDENT_INT (1);
          P3 (D, "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" &
                  " P3 (-1)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("SUBPROGRAM P3 WAS NOT CALLED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P3 (-1)");
     END; -- (D1)

     BEGIN  -- (E)
          CALLED := FALSE;
          D := 9;
          P4 (D, "10");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" &
                  " P4 (10)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("SUBPROGRAM P4 WAS NOT CALLED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P4 (10)");
     END;  -- (E)

     BEGIN -- (E1)
          CALLED := FALSE;
          D := 0;
          P4 (D, "-1");
          FAILED ("CONSTRAINT_ERROR NOT RAISED ON RETURN FROM" &
                  " P4 (-1)");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COUNT := COUNT + 1;
               IF NOT CALLED THEN
                    FAILED ("SUBPROGRAM P4 WAS NOT CALLED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED FOR P4 (-1)");
     END; -- (E1)

     IF (COUNT /= 8) THEN
          FAILED ("INCORRECT NUMBER OF CONSTRAINT_ERRORS RAISED");
     END IF;

     RESULT;

END C64104A;
