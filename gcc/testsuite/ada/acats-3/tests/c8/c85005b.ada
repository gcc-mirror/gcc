-- C85005B.ADA

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
--     CHECK THAT A VARIABLE CREATED BY A SUBPROGRAM 'IN OUT' FORMAL
--     PARAMETER CAN BE RENAMED AND HAS THE CORRECT VALUE, AND THAT
--     THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT AND PASSED
--     ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' PARAMETER,
--     AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT WHEN THE
--     VALUE OF THE RENAMED VARIABLE IS CHANGED, THE NEW VALUE IS
--     REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85005B IS

     TYPE ARRAY1 IS ARRAY (POSITIVE RANGE <>) OF INTEGER;
     TYPE RECORD1 (D : INTEGER) IS
          RECORD
               FIELD1 : INTEGER := 1;
          END RECORD;
     TYPE POINTER1 IS ACCESS INTEGER;

     PACKAGE PACK1 IS
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

     DI1 : INTEGER := 0;
     DA1 : ARRAY1(1..3) := (OTHERS => 0);
     DR1 : RECORD1(1) := (D => 1, FIELD1 => 0);
     DP1 : POINTER1 := NEW INTEGER'(0);
     DV1 : PACK1.PRIVY := PACK1.ZERO;
     DT1 : TASK1;

     I : INTEGER;

     GENERIC
          GI1 : IN OUT INTEGER;
          GA1 : IN OUT ARRAY1;
          GR1 : IN OUT RECORD1;
          GP1 : IN OUT POINTER1;
          GV1 : IN OUT PACK1.PRIVY;
          GT1 : IN OUT TASK1;
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

     PROCEDURE PROC (PI1 : IN OUT INTEGER; PA1 : IN OUT ARRAY1;
                     PR1 : IN OUT RECORD1; PP1 : IN OUT POINTER1;
                     PV1 : IN OUT PACK1.PRIVY; PT1 : IN OUT TASK1) IS
          XPI1 : INTEGER RENAMES PI1;
          XPA1 : ARRAY1 RENAMES PA1;
          XPR1 : RECORD1 RENAMES PR1;
          XPP1 : POINTER1 RENAMES PP1;
          XPV1 : PACK1.PRIVY RENAMES PV1;
          XPT1 : TASK1 RENAMES PT1;

          TASK TYPE TASK2 IS
               ENTRY ENTRY1 (TI1 : OUT INTEGER; TA1 : OUT ARRAY1;
                             TR1 : OUT RECORD1; TP1 : IN OUT POINTER1;
                             TV1 : IN OUT PACK1.PRIVY;
                             TT1 : IN OUT TASK1);
          END TASK2;

          CHK_TASK : TASK2;

          PROCEDURE PROC1 (PPI1 : IN OUT INTEGER; PPA1 : IN OUT ARRAY1;
                           PPR1 : IN OUT RECORD1; PPP1 : OUT POINTER1;
                           PPV1 : OUT PACK1.PRIVY;
                           PPT1 : IN OUT TASK1) IS
          BEGIN
               PPI1 := PPI1 + 1;
               PPA1 := (PPA1(1)+1, PPA1(2)+1, PPA1(3)+1);
               PPR1 := (D => 1, FIELD1 => PPR1.FIELD1 + 1);
               PPP1 := NEW INTEGER'(PP1.ALL + 1);
               PPV1 := PACK1.NEXT(PV1);
               PPT1.NEXT;
          END PROC1;

          TASK BODY TASK2 IS
          BEGIN
               ACCEPT ENTRY1 (TI1 : OUT INTEGER; TA1 : OUT ARRAY1;
                              TR1 : OUT RECORD1; TP1 : IN OUT POINTER1;
                              TV1 : IN OUT PACK1.PRIVY;
                              TT1 : IN OUT TASK1)
               DO
                    TI1 := PI1 + 1;
                    TA1 := (PA1(1)+1, PA1(2)+1, PA1(3)+1);
                    TR1 := (D => 1, FIELD1 => PR1.FIELD1 + 1);
                    TP1 := NEW INTEGER'(TP1.ALL + 1);
                    TV1 := PACK1.NEXT(TV1);
                    TT1.NEXT;
               END ENTRY1;
          END TASK2;

          PACKAGE GENPACK1 IS NEW GENERIC1
               (XPI1, XPA1, XPR1, XPP1, XPV1, XPT1);

     BEGIN
          IF XPI1 /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XPI1 (1)");
          END IF;

          IF XPA1 /= (IDENT_INT(1),IDENT_INT(1),IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XPA1 (1)");
          END IF;

          IF XPR1 /= (D => 1, FIELD1 => IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XPR1 (1)");
          END IF;

          IF XPP1 /= IDENT(PP1) OR XPP1.ALL /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XPP1 (1)");
          END IF;

          IF PACK1."/=" (XPV1, PACK1.IDENT(PACK1.ONE)) THEN
               FAILED ("INCORRECT VALUE OF XPV1 (1)");
          END IF;

          XPT1.VALU(I);
          IF I /= IDENT_INT(1) THEN
               FAILED ("INCORRECT RETURN VALUE OF XPT1.VALU (1)");
          END IF;

          PROC1(XPI1, XPA1, XPR1, XPP1, XPV1, XPT1);

          IF XPI1 /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XPI1 (2)");
          END IF;

          IF XPA1 /= (IDENT_INT(2),IDENT_INT(2),IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XPA1 (2)");
          END IF;

          IF XPR1 /= (D => 1, FIELD1 => IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XPR1 (2)");
          END IF;

          IF XPP1 /= IDENT(PP1) OR XPP1.ALL /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XPP1 (2)");
          END IF;

          IF PACK1."/=" (XPV1, PACK1.IDENT(PACK1.TWO)) THEN
               FAILED ("INCORRECT VALUE OF XPV1 (2)");
          END IF;

          XPT1.VALU(I);
          IF I /= IDENT_INT(2) THEN
               FAILED ("INCORRECT RETURN VALUE FROM XPT1.VALU (2)");
          END IF;

          CHK_TASK.ENTRY1 (XPI1, XPA1, XPR1, XPP1, XPV1, XPT1);

          IF XPI1 /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XPI1 (3)");
          END IF;

          IF XPA1 /= (IDENT_INT(3),IDENT_INT(3),IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XPA1 (3)");
          END IF;

          IF XPR1 /= (D => 1, FIELD1 => IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XPR1 (3)");
          END IF;

          IF XPP1 /= IDENT(PP1) OR XPP1.ALL /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XPP1 (3)");
          END IF;

          IF PACK1."/=" (XPV1, PACK1.IDENT(PACK1.THREE)) THEN
               FAILED ("INCORRECT VALUE OF XPV1 (3)");
          END IF;

          XPT1.VALU(I);
          IF I /= IDENT_INT(3) THEN
               FAILED ("INCORRECT RETURN VALUE OF XPT1.VALU (3)");
          END IF;

          XPI1 := XPI1 + 1;
          XPA1 := (XPA1(1)+1, XPA1(2)+1, XPA1(3)+1);
          XPR1 := (D => 1, FIELD1 => XPR1.FIELD1 + 1);
          XPP1 := NEW INTEGER'(XPP1.ALL + 1);
          XPV1 := PACK1.NEXT(XPV1);
          XPT1.NEXT;

          IF XPI1 /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XPI1 (4)");
          END IF;

          IF XPA1 /= (IDENT_INT(4),IDENT_INT(4),IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XPA1 (4)");
          END IF;

          IF XPR1 /= (D => 1, FIELD1 => IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XPR1 (4)");
          END IF;

          IF XPP1 /= IDENT(PP1) OR XPP1.ALL /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XPP1 (4)");
          END IF;

          IF PACK1."/=" (XPV1, PACK1.IDENT(PACK1.FOUR)) THEN
               FAILED ("INCORRECT VALUE OF XPV1 (4)");
          END IF;

          XPT1.VALU(I);
          IF I /= IDENT_INT(4) THEN
               FAILED ("INCORRECT RETURN VALUE OF XPT1.VALU (4)");
          END IF;

          PI1 := PI1 + 1;
          PA1 := (PA1(1)+1, PA1(2)+1, PA1(3)+1);
          PR1 := (D => 1, FIELD1 => PR1.FIELD1 + 1);
          PP1 := NEW INTEGER'(PP1.ALL + 1);
          PV1 := PACK1.NEXT(PV1);
          PT1.NEXT;

          IF XPI1 /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XPI1 (5)");
          END IF;

          IF XPA1 /= (IDENT_INT(5),IDENT_INT(5),IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XPA1 (5)");
          END IF;

          IF XPR1 /= (D => 1, FIELD1 => IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XPR1 (5)");
          END IF;

          IF XPP1 /= IDENT(PP1) OR XPP1.ALL /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XPP1 (5)");
          END IF;

          IF PACK1."/=" (XPV1, PACK1.IDENT(PACK1.FIVE)) THEN
               FAILED ("INCORRECT VALUE OF XPV1 (5)");
          END IF;

          XPT1.VALU(I);
          IF I /= IDENT_INT(5) THEN
               FAILED ("INCORRECT RETURN VALUE OF XPT1.VALU (5)");
          END IF;
     END PROC;

BEGIN
     TEST ("C85005B", "CHECK THAT A VARIABLE CREATED BY A SUBPROGRAM " &
                      "'IN OUT' FORMAL PARAMETER CAN BE RENAMED " &
                      "AND HAS THE CORRECT VALUE, AND THAT THE NEW " &
                      "NAME CAN BE USED IN AN ASSIGNMENT STATEMENT " &
                      "AND PASSED ON AS AN ACTUAL SUBPROGRAM OR " &
                      "ENTRY 'IN OUT' OR 'OUT' PARAMETER, AND AS AN " &
                      "ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT " &
                      "WHEN THE VALUE OF THE RENAMED VARIABLE IS " &
                      "CHANGED, THE NEW VALUE IS REFLECTED BY THE " &
                      "VALUE OF THE NEW NAME");

     PROC (DI1, DA1, DR1, DP1, DV1, DT1);

     DT1.STOP;

     RESULT;
END C85005B;
