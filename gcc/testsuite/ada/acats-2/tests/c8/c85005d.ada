-- C85005D.ADA

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
--     CHECK THAT A VARIABLE CREATED BY A GENERIC 'IN OUT' FORMAL
--     PARAMETER CAN BE RENAMED AND HAS THE CORRECT VALUE, AND
--     THAT THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT AND
--     PASSED ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT'
--     PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER,
--     AND THAT WHEN THE VALUE OF THE RENAMED VARIABLE IS CHANGED,
--     THE NEW VALUE IS REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85005D IS

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
          XGI1 : INTEGER RENAMES GI1;
          XGA1 : ARRAY1 RENAMES GA1;
          XGR1 : RECORD1 RENAMES GR1;
          XGP1 : POINTER1 RENAMES GP1;
          XGV1 : PACK1.PRIVY RENAMES GV1;
          XGT1 : TASK1 RENAMES GT1;

          TASK TYPE TASK2 IS
               ENTRY ENTRY1 (TI1 : OUT INTEGER; TA1 : OUT ARRAY1;
                             TR1 : OUT RECORD1; TP1 : IN OUT POINTER1;
                             TV1 : IN OUT PACK1.PRIVY;
                             TT1 : IN OUT TASK1);
          END TASK2;

          G_CHK_TASK : TASK2;

          GENERIC
               GGI1 : IN OUT INTEGER;
               GGA1 : IN OUT ARRAY1;
               GGR1 : IN OUT RECORD1;
               GGP1 : IN OUT POINTER1;
               GGV1 : IN OUT PACK1.PRIVY;
               GGT1 : IN OUT TASK1;
          PACKAGE GENERIC2 IS
          END GENERIC2;

          PACKAGE BODY GENERIC2 IS
          BEGIN
               GGI1 := GGI1 + 1;
               GGA1 := (GGA1(1)+1, GGA1(2)+1, GGA1(3)+1);
               GGR1 := (D => 1, FIELD1 => GGR1.FIELD1 + 1);
               GGP1 := NEW INTEGER'(GGP1.ALL + 1);
               GGV1 := PACK1.NEXT(GGV1);
               GGT1.NEXT;
          END GENERIC2;

          TASK BODY TASK2 IS
          BEGIN
               ACCEPT ENTRY1 (TI1 : OUT INTEGER; TA1 : OUT ARRAY1;
                              TR1 : OUT RECORD1; TP1 : IN OUT POINTER1;
                              TV1 : IN OUT PACK1.PRIVY;
                              TT1 : IN OUT TASK1)
               DO
                    TI1 := GI1 + 1;
                    TA1 := (GA1(1)+1, GA1(2)+1, GA1(3)+1);
                    TR1 := (D => 1, FIELD1 => GR1.FIELD1 + 1);
                    TP1 := NEW INTEGER'(TP1.ALL + 1);
                    TV1 := PACK1.NEXT(TV1);
                    TT1.NEXT;
               END ENTRY1;
          END TASK2;

          PROCEDURE PROC1 (PI1 : IN OUT INTEGER; PA1 : IN OUT ARRAY1;
                           PR1 : IN OUT RECORD1; PP1 : OUT POINTER1;
                           PV1 : OUT PACK1.PRIVY; PT1 : IN OUT TASK1) IS
          BEGIN
               PI1 := PI1 + 1;
               PA1 := (PA1(1)+1, PA1(2)+1, PA1(3)+1);
               PR1 := (D => 1, FIELD1 => PR1.FIELD1 + 1);
               PP1 := NEW INTEGER'(GP1.ALL + 1);
               PV1 := PACK1.NEXT(GV1);
               PT1.NEXT;
          END PROC1;

          PACKAGE GENPACK2 IS NEW GENERIC2
               (XGI1, XGA1, XGR1, XGP1, XGV1, XGT1);

     BEGIN
          IF XGI1 /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XGI1 (1)");
          END IF;

          IF XGA1 /= (IDENT_INT(1),IDENT_INT(1),IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XGA1 (1)");
          END IF;

          IF XGR1 /= (D => 1, FIELD1 => IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XGR1 (1)");
          END IF;

          IF XGP1 /= IDENT(GP1) OR XGP1.ALL /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XGP1 (1)");
          END IF;

          IF PACK1."/=" (XGV1, PACK1.IDENT(PACK1.ONE)) THEN
               FAILED ("INCORRECT VALUE OF XGV1 (1)");
          END IF;

          XGT1.VALU(I);
          IF I /= IDENT_INT(1) THEN
               FAILED ("INCORRECT RETURN VALUE OF XGT1.VALU (1)");
          END IF;

          PROC1(XGI1, XGA1, XGR1, XGP1, XGV1, XGT1);

          IF XGI1 /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XGI1 (2)");
          END IF;

          IF XGA1 /= (IDENT_INT(2),IDENT_INT(2),IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XGA1 (2)");
          END IF;

          IF XGR1 /= (D => 1, FIELD1 => IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XGR1 (2)");
          END IF;

          IF XGP1 /= IDENT(GP1) OR XGP1.ALL /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XGP1 (2)");
          END IF;

          IF PACK1."/=" (XGV1, PACK1.IDENT(PACK1.TWO)) THEN
               FAILED ("INCORRECT VALUE OF XGV1 (2)");
          END IF;

          XGT1.VALU(I);
          IF I /= IDENT_INT(2) THEN
               FAILED ("INCORRECT RETURN VALUE OF XGT1.VALU (2)");
          END IF;

          G_CHK_TASK.ENTRY1(XGI1, XGA1, XGR1, XGP1, XGV1, XGT1);

          IF XGI1 /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XGI1 (3)");
          END IF;

          IF XGA1 /= (IDENT_INT(3),IDENT_INT(3),IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XGA1 (3)");
          END IF;

          IF XGR1 /= (D => 1, FIELD1 => IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XGR1 (3)");
          END IF;

          IF XGP1 /= IDENT(GP1) OR XGP1.ALL /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XGP1 (3)");
          END IF;

          IF PACK1."/=" (XGV1, PACK1.IDENT(PACK1.THREE)) THEN
               FAILED ("INCORRECT VALUE OF XGV1 (3)");
          END IF;

          XGT1.VALU(I);
          IF I /= IDENT_INT(3) THEN
               FAILED ("INCORRECT RETURN VALUE OF XGT1.VALU (3)");
          END IF;

          XGI1 := XGI1 + 1;
          XGA1 := (XGA1(1)+1, XGA1(2)+1, XGA1(3)+1);
          XGR1 := (D => 1, FIELD1 => XGR1.FIELD1 + 1);
          XGP1 := NEW INTEGER'(XGP1.ALL + 1);
          XGV1 := PACK1.NEXT(XGV1);
          XGT1.NEXT;

          IF XGI1 /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XGI1 (4)");
          END IF;

          IF XGA1 /= (IDENT_INT(4),IDENT_INT(4),IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XGA1 (4)");
          END IF;

          IF XGR1 /= (D => 1, FIELD1 => IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XGR1 (4)");
          END IF;

          IF XGP1 /= IDENT(GP1) OR XGP1.ALL /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XGP1 (4)");
          END IF;

          IF PACK1."/=" (XGV1, PACK1.IDENT(PACK1.FOUR)) THEN
               FAILED ("INCORRECT VALUE OF XGV1 (4)");
          END IF;

          XGT1.VALU(I);
          IF I /= IDENT_INT(4) THEN
               FAILED ("INCORRECT RETURN VALUE OF XGT1.VALU (4)");
          END IF;

          GI1 := GI1 + 1;
          GA1 := (GA1(1)+1, GA1(2)+1, GA1(3)+1);
          GR1 := (D => 1, FIELD1 => GR1.FIELD1 + 1);
          GP1 := NEW INTEGER'(GP1.ALL + 1);
          GV1 := PACK1.NEXT(GV1);
          GT1.NEXT;

          IF XGI1 /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XGI1 (5)");
          END IF;

          IF XGA1 /= (IDENT_INT(5),IDENT_INT(5),IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XGA1 (5)");
          END IF;

          IF XGR1 /= (D => 1, FIELD1 => IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XGR1 (5)");
          END IF;

          IF XGP1 /= IDENT(GP1) OR XGP1.ALL /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XGP1 (5)");
          END IF;

          IF PACK1."/=" (XGV1, PACK1.IDENT(PACK1.FIVE)) THEN
               FAILED ("INCORRECT VALUE OF XGV1 (5)");
          END IF;

          XGT1.VALU(I);
          IF I /= IDENT_INT(5) THEN
               FAILED ("INCORRECT RETURN VALUE OF XGT1.VALU (5)");
          END IF;
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
     TEST ("C85005D", "CHECK THAT A VARIABLE CREATED BY A GENERIC " &
                      "'IN OUT' FORMAL PARAMETER CAN BE RENAMED " &
                      "AND HAS THE CORRECT VALUE, AND THAT THE NEW " &
                      "NAME CAN BE USED IN AN ASSIGNMENT STATEMENT " &
                      "AND PASSED ON AS AN ACTUAL SUBPROGRAM OR " &
                      "ENTRY 'IN OUT' OR 'OUT' PARAMETER, AND AS AN " &
                      "ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT " &
                      "WHEN THE VALUE OF THE RENAMED VARIABLE IS " &
                      "CHANGED, THE NEW VALUE IS REFLECTED BY THE " &
                      "VALUE OF THE NEW NAME");

     DECLARE
          PACKAGE GENPACK1 IS NEW
               GENERIC1 (DI1, DA1, DR1, DP1, DV1, DT1);
     BEGIN
          NULL;
     END;

     DT1.STOP;

     RESULT;
END C85005D;
