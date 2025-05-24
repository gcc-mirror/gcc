-- C64103B.ADA

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
--     CHECK THAT, FOR IN-OUT PARAMETERS OF A SCALAR TYPE,
--     CONSTRAINT_ERROR IS RAISED:
--          BEFORE A SUBPROGRAM CALL WHEN THE CONVERTED ACTUAL
--          PARAMETER IS OUTSIDE THE RANGE OF THE FORMAL PARAMETER'S
--          SUBTYPE;
--          AFTER A SUBPROGRAM CALL WHEN THE CONVERTED FORMAL PARAMETER
--          IS OUTSIDE THE RANGE OF THE ACTUAL PARAMETER'S SUBTYPE.

-- HISTORY:
--     CPP  07/18/84  CREATED ORIGINAL TEST.
--     VCL  10/27/87  MODIFIED THIS HEADER; ADDED STATEMENTS WHICH
--                    REFERENCED THE ACTUAL PARAMETERS IN THE SECOND
--                    SUBTEST.

WITH REPORT;  USE REPORT;
PROCEDURE C64103B IS
BEGIN
     TEST ("C64103B", "FOR IN-OUT PARAMETERS OF A SCALAR TYPE, " &
                      "CONSTRAINT_ERROR IS RAISED:  BEFORE A " &
                      "SUBPROGRAM CALL WHEN THE CONVERTED ACTUAL " &
                      "PARAMETER IS OUTSIDE THE RANGE OF THE FORMAL " &
                      "PARAMETER'S SUBTYPE;  AFTER A SUBPROGRAM " &
                      "CALL WHEN THE CONVERTED FORMAL PARAMETER IS " &
                      "OUTSIDE THE RANGE OF THE ACTUAL PARAMETER'S " &
                      "SUBTYPE");


     DECLARE
          A0 : INTEGER := -9;
          A1 : INTEGER := IDENT_INT(-1);
          TYPE SUBINT IS RANGE -8 .. -2;

          TYPE FLOAT_TYPE IS DIGITS 3 RANGE 0.0 .. 3.0;
          A2 : FLOAT_TYPE := 0.12;
          A3 : FLOAT_TYPE := 2.5;
          TYPE NEW_FLOAT IS DIGITS 3 RANGE 1.0 .. 2.0;

          TYPE FIXED_TYPE IS DELTA 1.0 RANGE -2.0 .. 5.0;
          A4 : FIXED_TYPE := -2.0;
          A5 : FIXED_TYPE := 4.0;
          TYPE NEW_FIXED IS DELTA 1.0 RANGE -1.0 .. 3.0;

          A6 : CHARACTER := 'A';
          SUBTYPE SUPER_CHAR IS CHARACTER RANGE 'B'..'Q';

          TYPE COLOR IS (RED, BURGUNDY, LILAC, MAROON, MAGENTA);
          SUBTYPE A_COLOR IS COLOR RANGE RED..LILAC;
          SUBTYPE B_COLOR IS COLOR RANGE MAROON..MAGENTA;
          A7 : B_COLOR := MAROON;

          PROCEDURE P1 (X : IN OUT SUBINT;
                        S :        STRING) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P1 (A" &
                       S & ")");
          END P1;

          PROCEDURE P2 (X : IN OUT NEW_FLOAT;
                        S :        STRING)     IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P2 (A" &
                       S & ")");
          END P2;

          PROCEDURE P3 (X : IN OUT NEW_FIXED;
                        S :        STRING)     IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P3 (A" &
                       S & ")");
          END P3;

          PROCEDURE P4 (X : IN OUT SUPER_CHAR;
                        S :        STRING)     IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P4 (A" &
                        S & ")");
          END P4;

          PROCEDURE P5 (X : IN OUT A_COLOR;
                        S :        STRING) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P5 (A" &
                       S & ")");
          END P5;
     BEGIN
          BEGIN
               P1 (SUBINT (A0), "1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (A1)");
          END;

          BEGIN
               P1 (SUBINT (A1), "2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (A2)");
          END;

          BEGIN
               P2 (NEW_FLOAT (A2), "1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P2 (A1)");
          END;

          BEGIN
               P2 (NEW_FLOAT (A3), "2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P2 (A2)");
          END;

          BEGIN
               P3 (NEW_FIXED (A4), "1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P3 (A1)");
          END;

          BEGIN
               P3 (NEW_FIXED (A5), "2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P3 (A2)");
          END;

          BEGIN
               P4 (SUPER_CHAR (A6),"1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P4 (A1)");
          END;

          BEGIN
               P5 (A_COLOR (A7), "1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P5 (A1)");
          END;
     END;


     DECLARE
          CALLED : BOOLEAN;
          TYPE SUBINT IS RANGE -8 .. -2;
          A0 : SUBINT := -3;
          A1 : INTEGER := -9;
          A2 : INTEGER := -1;

          TYPE FLOAT IS DIGITS 3 RANGE -1.0 .. 2.0;
          TYPE A_FLOAT IS DIGITS 3 RANGE 0.0 .. 1.0;
          A3 : A_FLOAT := 1.0;
          A4 : FLOAT := -0.5;
          A5 : FLOAT := 1.5;

          TYPE NEW_FIXED IS DELTA 1.0 RANGE -1.0 .. 3.0;
          A6 : NEW_FIXED := 0.0;
          TYPE FIXED_TYPE IS DELTA 1.0 RANGE -2.0 .. 5.0;
          A7 : FIXED_TYPE := -2.0;
          A8 : FIXED_TYPE := 4.0;

          SUBTYPE SUPER_CHAR IS CHARACTER RANGE 'B'..'Q';
          A9  : SUPER_CHAR := 'C';
          A10 : CHARACTER := 'A';
          A11 : CHARACTER := 'R';

          PROCEDURE P1 (X : IN OUT INTEGER; Y : INTEGER) IS
          BEGIN
               CALLED := TRUE;
               X := IDENT_INT (Y);
          END P1;

          PROCEDURE P2 (X : IN OUT FLOAT; Y : FLOAT) IS
          BEGIN
               CALLED := TRUE;
               X := Y;
          END P2;

          PROCEDURE P3 ( X : IN OUT FIXED_TYPE; Y : FIXED_TYPE) IS
          BEGIN
               CALLED := TRUE;
               X := Y;
          END P3;

          PROCEDURE P4 (X : IN OUT CHARACTER; Y : CHARACTER) IS
          BEGIN
               CALLED := TRUE;
               X := IDENT_CHAR(Y);
          END P4;
     BEGIN
          BEGIN
               CALLED := FALSE;
               P1 (INTEGER(A0), A1);
               IF A0 = -3 THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P1 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P1 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P1 (B1)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B1)");
          END;

          BEGIN
               CALLED := FALSE;
               P1 (INTEGER(A0), A2);
               IF A0 = -3 THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P1 (B3)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P1 (B4)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P1 (B2)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B2)");
          END;

          BEGIN
               CALLED := FALSE;
               P2 (FLOAT (A3), A4);
               IF A3 = 1.0 THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P2 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P2 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P2 (B1)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P2 (B1)");
          END;

          BEGIN
               CALLED := FALSE;
               P2 (FLOAT (A3), A5);
               IF A3 = 1.0 THEN
                    FAILED ("EXCEPTION NOT RAISED -P2 (B3)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED -P2 (B4)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P2 (B2)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P2 (B2)");
          END;

          BEGIN
               CALLED := FALSE;
               P3 (FIXED_TYPE (A6), A7);
               IF A6 = 0.0 THEN
                    FAILED ("EXCEPTION NOT RAISED -P3 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED -P3 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P3 (B1)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P3 (B1)");
          END;

          BEGIN
               CALLED := FALSE;
               P3 (FIXED_TYPE (A6), A8);
               IF A6 = 0.0 THEN
                    FAILED ("EXCEPTION NOT RAISED -P3 (B3)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED -P3 (B4)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P3 (B2)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P3 (B2)");
          END;

          BEGIN
               CALLED := FALSE;
               P4 (CHARACTER (A9), A10);
               IF A9 = 'C' THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P4 (B1)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P4 (B2)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P4 (B1)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P4 (B1)");
          END;

          BEGIN
               CALLED := FALSE;
               P4 (CHARACTER (A9), A11);
               IF A9 = 'C' THEN
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P4 (B3)");
               ELSE
                    FAILED ("EXCEPTION NOT RAISED AFTER CALL -P4 (B4)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF NOT CALLED THEN
                         FAILED ("EXCEPTION RAISED BEFORE CALL " &
                                 "-P4 (B2)");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P4 (B2)");
          END;
     END;

     RESULT;
END C64103B;
