-- C85005C.ADA

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
--     CHECK THAT A VARIABLE CREATED BY AN ENTRY 'IN OUT' FORMAL
--     PARAMETER CAN BE RENAMED AND HAS THE CORRECT VALUE, AND THAT
--     THE NEW NAME CAN BE USED IN AN ASSIGNMENT STATEMENT AND PASSED
--     ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' PARAMETER,
--     AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER, AND THAT WHEN THE
--     VALUE OF THE RENAMED VARIABLE IS CHANGED, THE NEW VALUE IS
--     REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85005C IS

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

BEGIN
     TEST ("C85005C", "CHECK THAT A VARIABLE CREATED BY AN ENTRY " &
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
          TASK MAIN_TASK IS
               ENTRY START (TI1 : IN OUT INTEGER; TA1 : IN OUT ARRAY1;
                            TR1 : IN OUT RECORD1; TP1 : IN OUT POINTER1;
                            TV1 : IN OUT PACK1.PRIVY;
                            TT1 : IN OUT TASK1);
          END MAIN_TASK;

          TASK BODY MAIN_TASK IS
          BEGIN
               ACCEPT START (TI1: IN OUT INTEGER; TA1: IN OUT ARRAY1;
                             TR1: IN OUT RECORD1; TP1: IN OUT POINTER1;
                             TV1: IN OUT PACK1.PRIVY;
                             TT1: IN OUT TASK1) DO
                    DECLARE
                         XTI1 : INTEGER RENAMES TI1;
                         XTA1 : ARRAY1 RENAMES TA1;
                         XTR1 : RECORD1 RENAMES TR1;
                         XTP1 : POINTER1 RENAMES TP1;
                         XTV1 : PACK1.PRIVY RENAMES TV1;
                         XTT1 : TASK1 RENAMES TT1;

                         TASK TYPE TASK2 IS
                              ENTRY ENTRY1 (TTI1 : OUT INTEGER;
                                            TTA1 : OUT ARRAY1;
                                            TTR1 : OUT RECORD1;
                                            TTP1 : IN OUT POINTER1;
                                            TTV1 : IN OUT PACK1.PRIVY;
                                            TTT1 : IN OUT TASK1);
                         END TASK2;

                         CHK_TASK : TASK2;

                         PROCEDURE PROC1 (PTI1 : IN OUT INTEGER;
                                          PTA1 : IN OUT ARRAY1;
                                          PTR1 : IN OUT RECORD1;
                                          PTP1 : OUT POINTER1;
                                          PTV1 : OUT PACK1.PRIVY;
                                          PTT1 : IN OUT TASK1) IS
                         BEGIN
                              PTI1 := PTI1 + 1;
                              PTA1 := (PTA1(1)+1, PTA1(2)+1, PTA1(3)+1);
                              PTR1 := (D => 1,
                                       FIELD1 => PTR1.FIELD1 + 1);
                              PTP1 := NEW INTEGER'(TP1.ALL + 1);
                              PTV1 := PACK1.NEXT(TV1);
                              PTT1.NEXT;
                         END PROC1;

                         TASK BODY TASK2 IS
                         BEGIN
                              ACCEPT ENTRY1 (TTI1 : OUT INTEGER;
                                             TTA1 : OUT ARRAY1;
                                             TTR1 : OUT RECORD1;
                                             TTP1 : IN OUT POINTER1;
                                             TTV1 : IN OUT PACK1.PRIVY;
                                             TTT1 : IN OUT TASK1)
                              DO
                                   TTI1 := TI1 + 1;
                                   TTA1 := (TA1(1)+1,
                                   TA1(2)+1, TA1(3)+1);
                                   TTR1 := (D => 1,
                                            FIELD1 => TR1.FIELD1 + 1);
                                   TTP1 := NEW INTEGER'(TTP1.ALL + 1);
                                   TTV1 := PACK1.NEXT(TTV1);
                                   TTT1.NEXT;
                              END ENTRY1;
                         END TASK2;

                         PACKAGE GENPACK1 IS NEW GENERIC1
                              (XTI1, XTA1, XTR1, XTP1, XTV1, XTT1);
                    BEGIN
                         IF XTI1 /= IDENT_INT(1) THEN
                              FAILED ("INCORRECT VALUE OF XTI1 (1)");
                         END IF;

                         IF XTA1 /= (IDENT_INT(1),IDENT_INT(1),
                                     IDENT_INT(1)) THEN
                              FAILED ("INCORRECT VALUE OF XTA1 (1)");
                         END IF;

                         IF XTR1 /= (D => 1, FIELD1 => IDENT_INT(1))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTR1 (1)");
                         END IF;

                         IF XTP1 /= IDENT(TP1) OR
                            XTP1.ALL /= IDENT_INT(1) THEN
                              FAILED ("INCORRECT VALUE OF XTP1 (1)");
                         END IF;

                         IF PACK1."/=" (XTV1, PACK1.IDENT(PACK1.ONE))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTV1 (1)");
                         END IF;

                         XTT1.VALU(I);
                         IF I /= IDENT_INT(1) THEN
                              FAILED ("INCORRECT RETURN VALUE OF " &
                                      "XTT1.VALU (1)");
                         END IF;

                         PROC1(XTI1, XTA1, XTR1, XTP1, XTV1, XTT1);

                         IF XTI1 /= IDENT_INT(2) THEN
                              FAILED ("INCORRECT VALUE OF XTI1 (2)");
                         END IF;

                         IF XTA1 /= (IDENT_INT(2),IDENT_INT(2),
                                     IDENT_INT(2)) THEN
                              FAILED ("INCORRECT VALUE OF XTA1 (2)");
                         END IF;

                         IF XTR1 /= (D => 1, FIELD1 => IDENT_INT(2))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTR1 (2)");
                         END IF;

                         IF XTP1 /= IDENT(TP1) OR
                            XTP1.ALL /= IDENT_INT(2) THEN
                              FAILED ("INCORRECT VALUE OF XTP1 (2)");
                         END IF;

                         IF PACK1."/=" (XTV1, PACK1.IDENT(PACK1.TWO))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTV1 (2)");
                         END IF;

                         XTT1.VALU(I);
                         IF I /= IDENT_INT(2) THEN
                              FAILED ("INCORRECT RETURN VALUE FROM " &
                                      "XTT1.VALU (2)");
                         END IF;

                         CHK_TASK.ENTRY1
                              (XTI1, XTA1, XTR1, XTP1, XTV1, XTT1);

                         IF XTI1 /= IDENT_INT(3) THEN
                              FAILED ("INCORRECT VALUE OF XTI1 (3)");
                         END IF;

                         IF XTA1 /= (IDENT_INT(3),IDENT_INT(3),
                                     IDENT_INT(3)) THEN
                              FAILED ("INCORRECT VALUE OF XTA1 (3)");
                         END IF;

                         IF XTR1 /= (D => 1, FIELD1 => IDENT_INT(3))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTR1 (3)");
                         END IF;

                         IF XTP1 /= IDENT(TP1) OR
                            XTP1.ALL /= IDENT_INT(3) THEN
                              FAILED ("INCORRECT VALUE OF XTP1 (3)");
                         END IF;

                         IF PACK1."/=" (XTV1, PACK1.IDENT(PACK1.THREE))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTV1 (3)");
                         END IF;

                         XTT1.VALU(I);
                         IF I /= IDENT_INT(3) THEN
                              FAILED ("INCORRECT RETURN VALUE OF " &
                                      "XTT1.VALU (3)");
                         END IF;

                         XTI1 := XTI1 + 1;
                         XTA1 := (XTA1(1)+1, XTA1(2)+1, XTA1(3)+1);
                         XTR1 := (D => 1, FIELD1 => XTR1.FIELD1 + 1);
                         XTP1 := NEW INTEGER'(XTP1.ALL + 1);
                         XTV1 := PACK1.NEXT(XTV1);
                         XTT1.NEXT;

                         IF XTI1 /= IDENT_INT(4) THEN
                              FAILED ("INCORRECT VALUE OF XTI1 (4)");
                         END IF;

                         IF XTA1 /= (IDENT_INT(4),IDENT_INT(4),
                                     IDENT_INT(4)) THEN
                              FAILED ("INCORRECT VALUE OF XTA1 (4)");
                         END IF;

                         IF XTR1 /= (D => 1, FIELD1 => IDENT_INT(4))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTR1 (4)");
                         END IF;

                         IF XTP1 /= IDENT(TP1) OR
                            XTP1.ALL /= IDENT_INT(4) THEN
                              FAILED ("INCORRECT VALUE OF XTP1 (4)");
                         END IF;

                         IF PACK1."/=" (XTV1, PACK1.IDENT(PACK1.FOUR))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTV1 (4)");
                         END IF;

                         XTT1.VALU(I);
                         IF I /= IDENT_INT(4) THEN
                              FAILED ("INCORRECT RETURN VALUE OF " &
                                      "XTT1.VALU (4)");
                         END IF;

                         TI1 := TI1 + 1;
                         TA1 := (TA1(1)+1, TA1(2)+1, TA1(3)+1);
                         TR1 := (D => 1, FIELD1 => TR1.FIELD1 + 1);
                         TP1 := NEW INTEGER'(TP1.ALL + 1);
                         TV1 := PACK1.NEXT(TV1);
                         TT1.NEXT;

                         IF XTI1 /= IDENT_INT(5) THEN
                              FAILED ("INCORRECT VALUE OF XTI1 (5)");
                         END IF;

                         IF XTA1 /= (IDENT_INT(5),IDENT_INT(5),
                                     IDENT_INT(5)) THEN
                              FAILED ("INCORRECT VALUE OF XTA1 (5)");
                         END IF;

                         IF XTR1 /= (D => 1, FIELD1 => IDENT_INT(5))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTR1 (5)");
                         END IF;

                         IF XTP1 /= IDENT(TP1) OR
                            XTP1.ALL /= IDENT_INT(5) THEN
                              FAILED ("INCORRECT VALUE OF XTP1 (5)");
                         END IF;

                         IF PACK1."/=" (XTV1, PACK1.IDENT(PACK1.FIVE))
                         THEN
                              FAILED ("INCORRECT VALUE OF XTV1 (5)");
                         END IF;

                         XTT1.VALU(I);
                         IF I /= IDENT_INT(5) THEN
                              FAILED ("INCORRECT RETURN VALUE OF " &
                                      "XTT1.VALU (5)");
                         END IF;
                    END;
               END START;
          END MAIN_TASK;

     BEGIN
          MAIN_TASK.START (DI1, DA1, DR1, DP1, DV1, DT1);
     END;

     DT1.STOP;

     RESULT;
END C85005C;
