-- C64103E.ADA

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
--     CHECK THAT, FOR IN-OUT PARAMETERS OF AN ACCESS TYPE,
--     CONSTRAINT_ERROR IS RAISED:
--          BEFORE A SUBPROGRAM CALL WHEN THE BOUNDS OR DISCRIMINANTS
--          OF THE ACTUAL DESIGNATED PARAMETER ARE DIFFERENT FROM
--          THOSE OF THE FORMAL DESIGNATED PARAMETER;
--          AFTER A SUBPROGRAM CALL WHEN THE BOUNDS OR DISCRIMINANTS
--          OF THE FORMAL DESIGNATED PARAMETER ARE DIFFERENT FROM
--          THOSE OF THE ACTUAL DESIGNATED PARAMETER.

-- HISTORY:
--     CPP  07/23/84  CREATED ORIGINAL TEST.
--     VCL  10/27/87  MODIFIED THIS HEADER; ADDED STATEMENTS WHICH
--                    REFERENCED THE ACTUAL PARAMETERS IN THE SECOND
--                    SUBTEST.

WITH REPORT;  USE REPORT;
PROCEDURE C64103E IS
BEGIN
     TEST ("C64103E", "FOR IN-OUT PARAMETERS OF AN ACCESS TYPE, " &
                      "CONSTRAINT_ERROR IS RAISED:  BEFORE A " &
                      "SUBPROGRAM CALL WHEN THE BOUNDS OR " &
                      "DISCRIMINANTS OF THE ACTUAL DESIGNATED " &
                      "PARAMETER ARE DIFFERENT FROM THOSE OF THE " &
                      "FORMAL DESIGNATED PARAMETER;  AFTER A " &
                      "SUBPROGRAM CALL WHEN THE BOUNDS OR " &
                      "DISCRIMINANTS OF THE FORMAL DESIGNATED " &
                      "PARAMETER ARE DIFFERENT FROM THOSE OF THE " &
                      "ACTUAL DESIGNATED PARAMETER");


     BEGIN
          DECLARE
               TYPE AST IS ACCESS STRING;
               SUBTYPE AST_3 IS AST(1..3);
               SUBTYPE AST_5 IS AST(3..5);
               X_3 : AST_3 := NEW STRING(1..IDENT_INT(3));

               PROCEDURE P1 (X : IN OUT AST_5) IS
               BEGIN
                    FAILED("EXCEPTION NOT RAISED BEFORE CALL -P1 (A)");
               END P1;
          BEGIN
               P1 (AST_5 (X_3));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (A)");
          END;

          DECLARE
               TYPE ARRAY_TYPE IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
               TYPE A_ARRAY IS ACCESS ARRAY_TYPE;
               SUBTYPE A1_ARRAY IS A_ARRAY (1..IDENT_INT(3));
               TYPE A2_ARRAY IS NEW A_ARRAY (2..4);
               A0 : A1_ARRAY := NEW ARRAY_TYPE (1..3);

               PROCEDURE P2 (X : IN OUT A2_ARRAY) IS
               BEGIN
                    FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P2 (A)");
               END P2;
          BEGIN
               P2 (A2_ARRAY (A0));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P2 (A)");
          END;

          DECLARE
               TYPE SUBINT IS RANGE 0..8;
               TYPE REC1 (DISC : SUBINT := 8) IS
                    RECORD
                         FIELD : SUBINT := DISC;
                    END RECORD;
               TYPE A1_REC IS ACCESS REC1;
               TYPE A2_REC IS NEW A1_REC(3);
               A0 : A1_REC := NEW REC1(4);

               PROCEDURE P3 (X : IN OUT A2_REC) IS
               BEGIN
                    FAILED ("EXCEPTION NOT RAISED BEFORE CALL " &
                            "-P3 (A)");
               END P3;

          BEGIN
               P3 (A2_REC (A0));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P3 (A)");
          END;

     END;


     BEGIN
          DECLARE
               TYPE AST IS ACCESS STRING;
               SUBTYPE AST_3 IS AST(IDENT_INT(1)..IDENT_INT(3));
               X_3 : AST_3 := NEW STRING'(1..IDENT_INT(3) => 'A');
               CALLED : BOOLEAN := FALSE;

               PROCEDURE P1 (X : IN OUT AST) IS
               BEGIN
                    CALLED := TRUE;
                    X := NEW STRING'(3..5 => 'C');
               END P1;
          BEGIN
               P1 (AST (X_3));
               IF X_3.ALL = STRING'(1 .. 3 => 'A') THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P1 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P1 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL" &
                                 "-P1 (B)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B)");
          END;

          DECLARE
               TYPE ARRAY_TYPE IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
               TYPE A_ARRAY IS ACCESS ARRAY_TYPE;
               SUBTYPE A1_ARRAY IS A_ARRAY (1..IDENT_INT(3));
               A0 : A1_ARRAY := NEW ARRAY_TYPE'(1..3 => TRUE);
               CALLED : BOOLEAN := FALSE;

               PROCEDURE P2 (X : IN OUT A_ARRAY) IS
               BEGIN
                    CALLED := TRUE;
                    X := NEW ARRAY_TYPE'(2..4 => FALSE);
               END P2;
          BEGIN
               P2 (A_ARRAY (A0));
               IF A0.ALL = ARRAY_TYPE'(1 .. 3 => TRUE) THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P2 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P2 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL" &
                                 "-P1 (B)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P2 (B)");
          END;

          DECLARE
               TYPE SUBINT IS RANGE 0..8;
               TYPE REC1 (DISC : SUBINT := 8) IS
                    RECORD
                         FIELD : SUBINT := DISC;
                    END RECORD;
               TYPE A1_REC IS ACCESS REC1;
               TYPE A2_REC IS NEW A1_REC;
               A0 : A1_REC(4) := NEW REC1(4);
               CALLED : BOOLEAN := FALSE;

               PROCEDURE P3 (X : IN OUT A2_REC) IS
               BEGIN
                    CALLED := TRUE;
                    X := NEW REC1;
               END P3;

          BEGIN
               P3 (A2_REC (A0));
               IF A0.ALL = REC1'(4,4) THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P3 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P3 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL" &
                                 "-P1 (B)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P3 (B)");
          END;

     END;

     RESULT;
END C64103E;
