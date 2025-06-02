-- C85005E.ADA

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
--     CHECK THAT A VARIABLE CREATED BY AN ALLOCATOR CAN BE RENAMED AND
--     HAS THE CORRECT VALUE, AND THAT THE NEW NAME CAN BE USED IN AN
--     ASSIGNMENT STATEMENT AND PASSED ON AS AN ACTUAL SUBPROGRAM OR
--     ENTRY 'IN OUT' OR 'OUT' PARAMETER, AND AS AN ACTUAL GENERIC
--     'IN OUT' PARAMETER, AND THAT WHEN THE VALUE OF THE RENAMED
--     VARIABLE IS CHANGED, THE NEW VALUE IS REFLECTED BY THE VALUE OF
--     THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85005E IS

     TYPE ARRAY1 IS ARRAY (POSITIVE RANGE <>) OF INTEGER;
     TYPE RECORD1 (D : INTEGER) IS
          RECORD
               FIELD1 : INTEGER := 1;
          END RECORD;
     TYPE POINTER1 IS ACCESS INTEGER;

     PACKAGE PACK1 IS
          TYPE PACKACC IS ACCESS INTEGER;
          AK1 : PACKACC := NEW INTEGER'(0);
          TYPE PRIVY IS PRIVATE;
          ZERO : CONSTANT PRIVY;
          ONE : CONSTANT PRIVY;
          TWO : CONSTANT PRIVY;
          THREE : CONSTANT PRIVY;
          FOUR : CONSTANT PRIVY;
          FIVE : CONSTANT PRIVY;
          FUNCTION IDENT (I : PRIVY) RETURN PRIVY;
          FUNCTION NEXT (I : PRIVY) RETURN PRIVY;
     PRIVATE
          TYPE PRIVY IS RANGE 0..127;
          ZERO : CONSTANT PRIVY := 0;
          ONE : CONSTANT PRIVY := 1;
          TWO : CONSTANT PRIVY := 2;
          THREE : CONSTANT PRIVY := 3;
          FOUR : CONSTANT PRIVY := 4;
          FIVE : CONSTANT PRIVY := 5;
     END PACK1;

     TASK TYPE TASK1 IS
          ENTRY ASSIGN (J : IN INTEGER);
          ENTRY VALU (J : OUT INTEGER);
          ENTRY NEXT;
          ENTRY STOP;
     END TASK1;

     GENERIC
          GI1 : IN OUT INTEGER;
          GA1 : IN OUT ARRAY1;
          GR1 : IN OUT RECORD1;
          GP1 : IN OUT POINTER1;
          GV1 : IN OUT PACK1.PRIVY;
          GT1 : IN OUT TASK1;
          GK1 : IN OUT INTEGER;
     PACKAGE GENERIC1 IS
     END GENERIC1;

     FUNCTION IDENT (P : POINTER1) RETURN POINTER1 IS
     BEGIN
          IF EQUAL (3,3) THEN
               RETURN P;
          ELSE
               RETURN NULL;
          END IF;
     END IDENT;

     PACKAGE BODY PACK1 IS
          FUNCTION IDENT (I : PRIVY) RETURN PRIVY IS
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN I;
               ELSE
                    RETURN PRIVY'(0);
               END IF;
          END IDENT;

          FUNCTION NEXT (I : PRIVY) RETURN PRIVY IS
          BEGIN
               RETURN I+1;
          END NEXT;
     END PACK1;

     PACKAGE BODY GENERIC1 IS
     BEGIN
          GI1 := GI1 + 1;
          GA1 := (GA1(1)+1, GA1(2)+1, GA1(3)+1);
          GR1 := (D => 1, FIELD1 => GR1.FIELD1 + 1);
          GP1 := NEW INTEGER'(GP1.ALL + 1);
          GV1 := PACK1.NEXT(GV1);
          GT1.NEXT;
          GK1 := GK1 + 1;
     END GENERIC1;

     TASK BODY TASK1 IS
          TASK_VALUE : INTEGER := 0;
          ACCEPTING_ENTRIES : BOOLEAN := TRUE;
     BEGIN
          WHILE ACCEPTING_ENTRIES LOOP
               SELECT
                    ACCEPT ASSIGN (J : IN INTEGER) DO
                         TASK_VALUE := J;
                    END ASSIGN;
               OR
                    ACCEPT VALU (J : OUT INTEGER) DO
                         J := TASK_VALUE;
                    END VALU;
               OR
                    ACCEPT NEXT DO
                         TASK_VALUE := TASK_VALUE + 1;
                    END NEXT;
               OR
                    ACCEPT STOP DO
                         ACCEPTING_ENTRIES := FALSE;
                    END STOP;
               END SELECT;
          END LOOP;
     END TASK1;

BEGIN
     TEST ("C85005E", "CHECK THAT A VARIABLE CREATED BY AN ALLOCATOR " &
                      "CAN BE RENAMED AND HAS THE CORRECT VALUE, AND " &
                      "THAT THE NEW NAME CAN BE USED IN AN ASSIGNMENT" &
                      " STATEMENT AND PASSED ON AS AN ACTUAL " &
                      "SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' " &
                      "PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' " &
                      "PARAMETER, AND THAT WHEN THE VALUE OF THE " &
                      "RENAMED VARIABLE IS CHANGED, THE NEW VALUE " &
                      "IS REFLECTED BY THE VALUE OF THE NEW NAME");

     DECLARE
          TYPE ACCINT IS ACCESS INTEGER;
          TYPE ACCARR IS ACCESS ARRAY1;
          TYPE ACCREC IS ACCESS RECORD1;
          TYPE ACCPTR IS ACCESS POINTER1;
          TYPE ACCPVT IS ACCESS PACK1.PRIVY;
          TYPE ACCTSK IS ACCESS TASK1;

          AI1 : ACCINT := NEW INTEGER'(0);
          AA1 : ACCARR := NEW ARRAY1'(0, 0, 0);
          AR1 : ACCREC := NEW RECORD1'(D => 1, FIELD1 => 0);
          AP1 : ACCPTR := NEW POINTER1'(NEW INTEGER'(0));
          AV1 : ACCPVT := NEW PACK1.PRIVY'(PACK1.ZERO);
          AT1 : ACCTSK := NEW TASK1;

          XAI1 : INTEGER RENAMES AI1.ALL;
          XAA1 : ARRAY1 RENAMES AA1.ALL;
          XAR1 : RECORD1 RENAMES AR1.ALL;
          XAP1 : POINTER1 RENAMES AP1.ALL;
          XAV1 : PACK1.PRIVY RENAMES AV1.ALL;
          XAK1 : INTEGER RENAMES PACK1.AK1.ALL;
          XAT1 : TASK1 RENAMES AT1.ALL;

          TASK TYPE TASK2 IS
               ENTRY ENTRY1 (TI1 : OUT INTEGER; TA1 : OUT ARRAY1;
                             TR1 : OUT RECORD1; TP1 : IN OUT POINTER1;
                             TV1 : IN OUT PACK1.PRIVY;
                             TT1 : IN OUT TASK1; TK1 : IN OUT INTEGER);
          END TASK2;

          I : INTEGER;
          A_CHK_TASK : TASK2;

          PROCEDURE PROC1 (PI1 : IN OUT INTEGER; PA1 : IN OUT ARRAY1;
                           PR1 : IN OUT RECORD1; PP1 : OUT POINTER1;
                           PV1 : OUT PACK1.PRIVY; PT1 : IN OUT TASK1;
                           PK1 : OUT INTEGER) IS

          BEGIN
               PI1 := PI1 + 1;
               PA1 := (PA1(1)+1, PA1(2)+1, PA1(3)+1);
               PR1 := (D => 1, FIELD1 => PR1.FIELD1 + 1);
               PP1 := NEW INTEGER'(AP1.ALL.ALL + 1);
               PV1 := PACK1.NEXT(AV1.ALL);
               PT1.NEXT;
               PK1 := PACK1.AK1.ALL + 1;
          END PROC1;

          TASK BODY TASK2 IS
          BEGIN
               ACCEPT ENTRY1 (TI1 : OUT INTEGER; TA1 : OUT ARRAY1;
                              TR1 : OUT RECORD1; TP1 : IN OUT POINTER1;
                              TV1 : IN OUT PACK1.PRIVY;
                              TT1 : IN OUT TASK1;
                              TK1 : IN OUT INTEGER) DO
                    TI1 := AI1.ALL + 1;
                    TA1 := (AA1.ALL(1)+1, AA1.ALL(2)+1, AA1.ALL(3)+1);
                    TR1 := (D => 1, FIELD1 => AR1.ALL.FIELD1 + 1);
                    TP1 := NEW INTEGER'(TP1.ALL + 1);
                    TV1 := PACK1.NEXT(TV1);
                    TT1.NEXT;
                    TK1 := TK1 + 1;
               END ENTRY1;
          END TASK2;

          PACKAGE GENPACK2 IS NEW
               GENERIC1 (XAI1, XAA1, XAR1, XAP1, XAV1, XAT1, XAK1);

     BEGIN
          IF XAI1 /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XAI1 (1)");
          END IF;

          IF XAA1 /= (IDENT_INT(1),IDENT_INT(1),IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XAA1 (1)");
          END IF;

          IF XAR1 /= (D => 1, FIELD1 => IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XAR1 (1)");
          END IF;

          IF XAP1 /= IDENT(AP1.ALL) OR XAP1.ALL /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XAP1 (1)");
          END IF;

          IF PACK1."/=" (XAV1, PACK1.IDENT(PACK1.ONE)) THEN
               FAILED ("INCORRECT VALUE OF XAV1 (1)");
          END IF;

          XAT1.VALU(I);
          IF I /= IDENT_INT(1) THEN
               FAILED ("INCORRECT RETURN VALUE OF XAT1.VALU (1)");
          END IF;

          IF XAK1 /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XAK1 (1)");
          END IF;

          PROC1(XAI1, XAA1, XAR1, XAP1, XAV1, XAT1, XAK1);

          IF XAI1 /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XAI1 (2)");
          END IF;

          IF XAA1 /= (IDENT_INT(2),IDENT_INT(2),IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XAA1 (2)");
          END IF;

          IF XAR1 /= (D => 1, FIELD1 => IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XAR1 (2)");
          END IF;

          IF XAP1 /= IDENT(AP1.ALL) OR XAP1.ALL /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XAP1 (2)");
          END IF;

          IF PACK1."/=" (XAV1, PACK1.IDENT(PACK1.TWO)) THEN
               FAILED ("INCORRECT VALUE OF XAV1 (2)");
          END IF;

          XAT1.VALU(I);
          IF I /= IDENT_INT(2) THEN
               FAILED ("INCORRECT RETURN VALUE OF XAT1.VALU (2)");
          END IF;

          IF XAK1 /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XAK1 (2)");
          END IF;

          A_CHK_TASK.ENTRY1(XAI1, XAA1, XAR1, XAP1, XAV1, XAT1, XAK1);

          IF XAI1 /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XAI1 (3)");
          END IF;

          IF XAA1 /= (IDENT_INT(3),IDENT_INT(3),IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XAA1 (3)");
          END IF;

          IF XAR1 /= (D => 1, FIELD1 => IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XAR1 (3)");
          END IF;

          IF XAP1 /= IDENT(AP1.ALL) OR XAP1.ALL /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XAP1 (3)");
          END IF;

          IF PACK1."/=" (XAV1, PACK1.IDENT(PACK1.THREE)) THEN
               FAILED ("INCORRECT VALUE OF XAV1 (3)");
          END IF;

          XAT1.VALU(I);
          IF I /= IDENT_INT(3) THEN
               FAILED ("INCORRECT RETURN VALUE OF XAT1.VALU (3)");
          END IF;

          IF XAK1 /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XAK1 (3)");
          END IF;

          XAI1 := XAI1 + 1;
          XAA1 := (XAA1(1)+1, XAA1(2)+1, XAA1(3)+1);
          XAR1 := (D => 1, FIELD1 => XAR1.FIELD1 + 1);
          XAP1 := NEW INTEGER'(XAP1.ALL + 1);
          XAV1 := PACK1.NEXT(XAV1);
          XAT1.NEXT;
          XAK1 := XAK1 + 1;

          IF XAI1 /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XAI1 (4)");
          END IF;

          IF XAA1 /= (IDENT_INT(4),IDENT_INT(4),IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XAA1 (4)");
          END IF;

          IF XAR1 /= (D => 1, FIELD1 => IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XAR1 (4)");
          END IF;

          IF XAP1 /= IDENT(AP1.ALL) OR XAP1.ALL /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XAP1 (4)");
          END IF;

          IF PACK1."/=" (XAV1, PACK1.IDENT(PACK1.FOUR)) THEN
               FAILED ("INCORRECT VALUE OF XAV1 (4)");
          END IF;

          XAT1.VALU(I);
          IF I /= IDENT_INT(4) THEN
               FAILED ("INCORRECT RETURN VALUE OF XAT1.VALU (4)");
          END IF;

          IF XAK1 /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XAK1 (4)");
          END IF;

          AI1.ALL := AI1.ALL + 1;
          AA1.ALL := (AA1.ALL(1)+1, AA1.ALL(2)+1, AA1.ALL(3)+1);
          AR1.ALL := (D => 1, FIELD1 => AR1.ALL.FIELD1 + 1);
          AP1.ALL := NEW INTEGER'(AP1.ALL.ALL + 1);
          AV1.ALL := PACK1.NEXT(AV1.ALL);
          AT1.NEXT;
          PACK1.AK1.ALL := PACK1.AK1.ALL + 1;

          IF XAI1 /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XAI1 (5)");
          END IF;

          IF XAA1 /= (IDENT_INT(5),IDENT_INT(5),IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XAA1 (5)");
          END IF;

          IF XAR1 /= (D => 1, FIELD1 => IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XAR1 (5)");
          END IF;

          IF XAP1 /= IDENT(AP1.ALL) OR XAP1.ALL /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XAP1 (5)");
          END IF;

          IF PACK1."/=" (XAV1, PACK1.IDENT(PACK1.FIVE)) THEN
               FAILED ("INCORRECT VALUE OF XAV1 (5)");
          END IF;

          XAT1.VALU(I);
          IF I /= IDENT_INT(5) THEN
               FAILED ("INCORRECT RETURN VALUE OF XAT1.VALU (5)");
          END IF;

          IF XAK1 /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XAK1 (5)");
          END IF;

          AT1.STOP;
     END;

     RESULT;
END C85005E;
