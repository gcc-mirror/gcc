-- C85014B.ADA

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
--     CHECK THAT THE BASE TYPE OF THE FORMAL PARAMETER AND THE RESULT
--     TYPE ARE USED TO DETERMINE WHICH SUBPROGRAM OR ENTRY IS BEING
--     RENAMED.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85014B IS

     TYPE INT IS NEW INTEGER;
     SUBTYPE SUBINT0 IS INT RANGE 0..INT'LAST;
     SUBTYPE SUBINT1 IS INT RANGE 1..INT'LAST;

     TASK TYPE T1 IS
          ENTRY ENTER (I1: IN OUT INTEGER);
          ENTRY STOP;
     END T1;

     TASK TYPE T2 IS
          ENTRY ENTER (I1: IN OUT INT);
          ENTRY STOP;
     END T2;

     TASK1 : T1;
     TASK2 : T2;

     FUNCTION F RETURN T1 IS
     BEGIN
          RETURN TASK1;
     END F;

     FUNCTION F RETURN T2 IS
     BEGIN
          RETURN TASK2;
     END F;

     PROCEDURE PROC (I1: IN OUT INTEGER) IS
     BEGIN
          I1 := I1 + 1;
     END PROC;

     PROCEDURE PROC (I1: IN OUT INT) IS
     BEGIN
          I1 := I1 + 2;
     END PROC;

     FUNCTION FUNK (I1: INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN I1 + 1;
     END FUNK;

     FUNCTION FUNK (I1: INTEGER) RETURN INT IS
     BEGIN
          RETURN INT(I1) + 2;
     END FUNK;

     FUNCTION FUNKX (N : NATURAL) RETURN POSITIVE IS
     BEGIN
          RETURN N + 1;
     END FUNKX;

     FUNCTION FUNKX (N : SUBINT0) RETURN SUBINT1 IS
     BEGIN
          RETURN N + 2;
     END FUNKX;

     TASK BODY T1 IS
          ACCEPTING_ENTRIES : BOOLEAN := TRUE;
     BEGIN
          WHILE ACCEPTING_ENTRIES LOOP
               SELECT
                    ACCEPT ENTER (I1 : IN OUT INTEGER) DO
                         I1 := I1 + 1;
                    END ENTER;
               OR
                    ACCEPT STOP DO
                         ACCEPTING_ENTRIES := FALSE;
                    END STOP;
               END SELECT;
          END LOOP;
     END T1;

     TASK BODY T2 IS
          ACCEPTING_ENTRIES : BOOLEAN := TRUE;
     BEGIN
          WHILE ACCEPTING_ENTRIES LOOP
               SELECT
                    ACCEPT ENTER (I1 : IN OUT INT) DO
                         I1 := I1 + 2;
                    END ENTER;
               OR
                    ACCEPT STOP DO
                         ACCEPTING_ENTRIES := FALSE;
                    END STOP;
               END SELECT;
          END LOOP;
     END T2;

BEGIN
     TEST ("C85014B", "CHECK THAT THE BASE TYPE OF THE FORMAL " &
                      "PARAMETER AND THE RESULT TYPE ARE USED TO " &
                      "DETERMINE WHICH SUBPROGRAM OR ENTRY IS BEING " &
                      "RENAMED");

     DECLARE
          PROCEDURE PROC1 (J1: IN OUT INTEGER) RENAMES PROC;
          PROCEDURE PROC2 (J1: IN OUT INT) RENAMES PROC;

          FUNCTION FUNK1 (J1: INTEGER) RETURN INTEGER RENAMES FUNK;
          FUNCTION FUNK2 (J1: INTEGER) RETURN INT RENAMES FUNK;

          PROCEDURE ENTRY1 (J1: IN OUT INTEGER) RENAMES F.ENTER;
          PROCEDURE ENTRY2 (J1: IN OUT INT) RENAMES F.ENTER;

          FUNCTION FUNK3 (J1: POSITIVE) RETURN NATURAL RENAMES FUNKX;
          FUNCTION FUNK4 (J1: SUBINT1) RETURN SUBINT0 RENAMES FUNKX;

          K1 : INTEGER := 0;
          K2 : INT := 0;
     BEGIN
          PROC1(K1);
          IF K1 /= IDENT_INT(1) THEN
               FAILED("INCORRECT RETURN VALUE FROM PROC1");
          END IF;

          K1 := FUNK1(K1);
          IF K1 /= IDENT_INT(2) THEN
               FAILED("INCORRECT RETURN VALUE FROM FUNK1");
          END IF;

          ENTRY1(K1);
          IF K1 /= IDENT_INT(3) THEN
               FAILED("INCORRECT RETURN VALUE FROM ENTRY1");
          END IF;

          K1 := FUNK3(K1);
          IF K1 /= IDENT_INT(4) THEN
               FAILED("INCORRECT RETURN VALUE FROM FUNK3");
          END IF;

          PROC2(K2);
          IF INTEGER(K2) /= IDENT_INT(2) THEN
               FAILED("INCORRECT RETURN VALUE FROM PROC2");
          END IF;

          K2 := FUNK2(INTEGER(K2));
          IF INTEGER(K2) /= IDENT_INT(4) THEN
               FAILED("INCORRECT RETURN VALUE FROM FUNK2");
          END IF;

          ENTRY2(K2);
          IF INTEGER(K2) /= IDENT_INT(6) THEN
               FAILED("INCORRECT RETURN VALUE FROM ENTRY2");
          END IF;

          K2 := FUNK4(K2);
          IF INTEGER(K2) /= IDENT_INT(8) THEN
               FAILED("INCORRECT RETURN VALUE FROM FUNK4");
          END IF;
     END;

     TASK1.STOP;
     TASK2.STOP;

     RESULT;
END C85014B;
