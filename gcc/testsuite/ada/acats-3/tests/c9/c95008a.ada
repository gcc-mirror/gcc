-- C95008A.ADA

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
-- CHECK THAT THE EXCEPTION CONSTRAINT_ERROR IS RAISED FOR AN
--   OUT-OF-RANGE INDEX VALUE WHEN REFERENCING AN ENTRY FAMILY,
--   EITHER IN AN ACCEPT_STATEMENT OR IN AN ENTRY_CALL.

-- SUBTESTS ARE:
--   (A)  INTEGER TYPE, STATIC LOWER BOUND, NO PARAMETERS.
--   (B)  CHARACTER TYPE, DYNAMIC UPPER BOUND, NO PARAMETERS.
--   (C)  BOOLEAN TYPE, STATIC NULL RANGE, NO PARAMETERS.
--   (D)  USER-DEFINED ENUMERATED TYPE, DYNAMIC LOWER BOUND, ONE
--           PARAMETER.
--   (E)  DERIVED INTEGER TYPE, DYNAMIC NULL RANGE, ONE PARAMETER.
--   (F)  DERIVED USER-DEFINED ENUMERATED TYPE, STATIC UPPER BOUND,
--           ONE PARAMETER.

-- JRK 11/4/81
-- JBG 11/11/84
-- SAIC 11/14/95 fixed test for 2.0.1

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C95008A IS

     C_E_NOT_RAISED : BOOLEAN;
     WRONG_EXC_RAISED : BOOLEAN;

BEGIN
     TEST ("C95008A", "OUT-OF-RANGE ENTRY FAMILY INDICES IN " &
                      "ACCEPT_STATEMENTS AND ENTRY_CALLS");

     --------------------------------------------------

     C_E_NOT_RAISED := FALSE;
     WRONG_EXC_RAISED := FALSE;

     DECLARE -- (A)

          TASK T IS
               ENTRY E (1..10);
               ENTRY CONTINUE;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT CONTINUE;
               SELECT
                    ACCEPT E (0);
               OR
                    DELAY 1.0 * Impdef.One_Second;
               END SELECT;
               C_E_NOT_RAISED := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    WRONG_EXC_RAISED := TRUE;
          END T;

     BEGIN -- (A)

          SELECT
               T.E (0);
          OR
               DELAY 15.0 * Impdef.One_Second;
          END SELECT;
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ENTRY_CALL - (A)");
          T.CONTINUE;

     EXCEPTION -- (A)

          WHEN CONSTRAINT_ERROR =>
               T.CONTINUE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN " &
                       "ENTRY_CALL - (A)");
               T.CONTINUE;

     END; -- (A)

     IF C_E_NOT_RAISED THEN
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ACCEPT_STATEMENT - (A)");
     END IF;

     IF WRONG_EXC_RAISED THEN
          FAILED ("WRONG EXCEPTION RAISED IN " &
                  "ACCEPT_STATEMENT - (A)");
     END IF;

     --------------------------------------------------

     C_E_NOT_RAISED := FALSE;
     WRONG_EXC_RAISED := FALSE;

     DECLARE -- (B)

          TASK T IS
               ENTRY E (CHARACTER RANGE 'A'..'Y');
               ENTRY CONTINUE;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT CONTINUE;
               SELECT
                    ACCEPT E (IDENT_CHAR('Z'));
               OR
                    DELAY 1.0 * Impdef.One_Second;
               END SELECT;
               C_E_NOT_RAISED := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    WRONG_EXC_RAISED := TRUE;
          END T;

     BEGIN -- (B)

          SELECT
               T.E (IDENT_CHAR('Z'));
          OR
               DELAY 15.0 * Impdef.One_Second;
          END SELECT;
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ENTRY_CALL - (B)");
          T.CONTINUE;

     EXCEPTION -- (B)

          WHEN CONSTRAINT_ERROR =>
               T.CONTINUE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN " &
                       "ENTRY_CALL - (B)");
               T.CONTINUE;

     END; -- (B)

     IF C_E_NOT_RAISED THEN
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ACCEPT_STATEMENT - (B)");
     END IF;

     IF WRONG_EXC_RAISED THEN
          FAILED ("WRONG EXCEPTION RAISED IN " &
                  "ACCEPT_STATEMENT - (B)");
     END IF;

     --------------------------------------------------

     C_E_NOT_RAISED := FALSE;
     WRONG_EXC_RAISED := FALSE;

     DECLARE -- (C)

          TASK T IS
               ENTRY E (TRUE..FALSE);
               ENTRY CONTINUE;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT CONTINUE;
               SELECT
                    ACCEPT E (FALSE);
               OR
                    DELAY 1.0 * Impdef.One_Second;
               END SELECT;
               C_E_NOT_RAISED := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    WRONG_EXC_RAISED := TRUE;
          END T;

     BEGIN -- (C)

          SELECT
               T.E (TRUE);
          OR
               DELAY 15.0 * Impdef.One_Second;
          END SELECT;
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ENTRY_CALL - (C)");
          T.CONTINUE;

     EXCEPTION -- (C)

          WHEN CONSTRAINT_ERROR =>
               T.CONTINUE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN " &
                       "ENTRY_CALL - (C)");
               T.CONTINUE;

     END; -- (C)

     IF C_E_NOT_RAISED THEN
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ACCEPT_STATEMENT - (C)");
     END IF;

     IF WRONG_EXC_RAISED THEN
          FAILED ("WRONG EXCEPTION RAISED IN " &
                  "ACCEPT_STATEMENT - (C)");
     END IF;

     --------------------------------------------------

     C_E_NOT_RAISED := FALSE;
     WRONG_EXC_RAISED := FALSE;

     DECLARE -- (D)

          TYPE ET IS (E0, E1, E2);
          DLB : ET := ET'VAL (IDENT_INT(1));      -- E1.

          TASK T IS
               ENTRY E (ET RANGE DLB..E2) (I : INTEGER);
               ENTRY CONTINUE;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT CONTINUE;
               SELECT
                    ACCEPT E (E0) (I : INTEGER);
               OR
                    DELAY 1.0 * Impdef.One_Second;
               END SELECT;
               C_E_NOT_RAISED := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    WRONG_EXC_RAISED := TRUE;
          END T;

     BEGIN -- (D)

          SELECT
               T.E (E0) (0);
          OR
               DELAY 15.0 * Impdef.One_Second;
          END SELECT;
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ENTRY_CALL - (D)");
          T.CONTINUE;

     EXCEPTION -- (D)

          WHEN CONSTRAINT_ERROR =>
               T.CONTINUE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN " &
                       "ENTRY_CALL - (D)");
               T.CONTINUE;

     END; -- (D)

     IF C_E_NOT_RAISED THEN
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ACCEPT_STATEMENT - (D)");
     END IF;

     IF WRONG_EXC_RAISED THEN
          FAILED ("WRONG EXCEPTION RAISED IN " &
                  "ACCEPT_STATEMENT - (D)");
     END IF;

     --------------------------------------------------

     C_E_NOT_RAISED := FALSE;
     WRONG_EXC_RAISED := FALSE;

     DECLARE -- (E)

          TYPE D_I IS NEW INTEGER;
          SUBTYPE DI IS D_I RANGE 3 .. D_I(IDENT_INT(2));

          TASK T IS
               ENTRY E (DI) (I : INTEGER);
               ENTRY CONTINUE;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT CONTINUE;
               SELECT
                    ACCEPT E (D_I(3)) (I : INTEGER);
               OR
                    DELAY 1.0 * Impdef.One_Second;
               END SELECT;
               C_E_NOT_RAISED := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    WRONG_EXC_RAISED := TRUE;
          END T;

     BEGIN -- (E)

          SELECT
               T.E (D_I(2)) (0);
          OR
               DELAY 15.0 * Impdef.One_Second;
          END SELECT;
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ENTRY_CALL - (E)");
          T.CONTINUE;

     EXCEPTION -- (E)

          WHEN CONSTRAINT_ERROR =>
               T.CONTINUE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN " &
                       "ENTRY_CALL - (E)");
               T.CONTINUE;

     END; -- (E)

     IF C_E_NOT_RAISED THEN
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ACCEPT_STATEMENT - (E)");
     END IF;

     IF WRONG_EXC_RAISED THEN
          FAILED ("WRONG EXCEPTION RAISED IN " &
                  "ACCEPT_STATEMENT - (E)");
     END IF;

     --------------------------------------------------

     C_E_NOT_RAISED := FALSE;
     WRONG_EXC_RAISED := FALSE;

     DECLARE -- (F)

          TYPE ET IS (E0, E1, E2);
          TYPE D_ET IS NEW ET;

          TASK T IS
               ENTRY E (D_ET RANGE E0..E1) (I : INTEGER);
               ENTRY CONTINUE;
          END T;

          TASK BODY T IS
          BEGIN
               ACCEPT CONTINUE;
               SELECT
                    ACCEPT E (D_ET'(E2)) (I : INTEGER);
               OR
                    DELAY 1.0 * Impdef.One_Second;
               END SELECT;
               C_E_NOT_RAISED := TRUE;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    WRONG_EXC_RAISED := TRUE;
          END T;

     BEGIN -- (F)

          SELECT
               T.E (D_ET'(E2)) (0);
          OR
               DELAY 15.0 * Impdef.One_Second;
          END SELECT;
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ENTRY_CALL - (F)");
          T.CONTINUE;

     EXCEPTION -- (F)

          WHEN CONSTRAINT_ERROR =>
               T.CONTINUE;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED IN " &
                       "ENTRY_CALL - (F)");
               T.CONTINUE;

     END; -- (F)

     IF C_E_NOT_RAISED THEN
          FAILED ("CONSTRAINT_ERROR NOT RAISED IN " &
                  "ACCEPT_STATEMENT - (F)");
     END IF;

     IF WRONG_EXC_RAISED THEN
          FAILED ("WRONG EXCEPTION RAISED IN " &
                  "ACCEPT_STATEMENT - (F)");
     END IF;

     --------------------------------------------------

     RESULT;
END C95008A;
