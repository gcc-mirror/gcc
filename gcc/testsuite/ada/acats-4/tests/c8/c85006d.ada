-- C85006D.ADA

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
--     CHECK THAT A COMPONENT OR SLICE OF A VARIABLE CREATED BY A
--     GENERIC 'IN OUT' FORMAL PARAMETER CAN BE RENAMED AND HAS THE
--     CORRECT VALUE, AND THAT THE NEW NAME CAN BE USED IN AN ASSIGNMENT
--     STATEMENT AND PASSED ON AS AN ACTUAL SUBPROGRAM OR ENTRY 'IN OUT'
--     OR 'OUT' PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' PARAMETER,
--     AND THAT WHEN THE VALUE OF THE RENAMED VARIABLE IS CHANGED,
--     THE NEW VALUE IS REFLECTED BY THE VALUE OF THE NEW NAME.

-- HISTORY:
--     JET 03/22/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85006D IS

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

     TYPE ARR_INT IS ARRAY(POSITIVE RANGE <>) OF INTEGER;
     TYPE ARR_ARR IS ARRAY(POSITIVE RANGE <>) OF ARRAY1(1..3);
     TYPE ARR_REC IS ARRAY(POSITIVE RANGE <>) OF RECORD1(1);
     TYPE ARR_PTR IS ARRAY(POSITIVE RANGE <>) OF POINTER1;
     TYPE ARR_PVT IS ARRAY(POSITIVE RANGE <>) OF PACK1.PRIVY;
     TYPE ARR_TSK IS ARRAY(POSITIVE RANGE <>) OF TASK1;

     TYPE REC_TYPE IS RECORD
          RI1 : INTEGER := 0;
          RA1 : ARRAY1(1..3) := (OTHERS => 0);
          RR1 : RECORD1(1) := (D => 1, FIELD1 => 0);
          RP1 : POINTER1 := NEW INTEGER'(0);
          RV1 : PACK1.PRIVY := PACK1.ZERO;
          RT1 : TASK1;
     END RECORD;

     DREC : REC_TYPE;

     DAI1 : ARR_INT(1..8) := (OTHERS => 0);
     DAA1 : ARR_ARR(1..8) := (OTHERS => (OTHERS => 0));
     DAR1 : ARR_REC(1..8) := (OTHERS => (D => 1, FIELD1 => 0));
     DAP1 : ARR_PTR(1..8) := (OTHERS => NEW INTEGER'(0));
     DAV1 : ARR_PVT(1..8) := (OTHERS => PACK1.ZERO);
     DAT1 : ARR_TSK(1..8);

     GENERIC
          REC : IN OUT REC_TYPE;
          AI1 : IN OUT ARR_INT;
          AA1 : IN OUT ARR_ARR;
          AR1 : IN OUT ARR_REC;
          AP1 : IN OUT ARR_PTR;
          AV1 : IN OUT ARR_PVT;
          AT1 : IN OUT ARR_TSK;
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
          XRI1 : INTEGER RENAMES REC.RI1;
          XRA1 : ARRAY1 RENAMES REC.RA1;
          XRR1 : RECORD1 RENAMES REC.RR1;
          XRP1 : POINTER1 RENAMES REC.RP1;
          XRV1 : PACK1.PRIVY RENAMES REC.RV1;
          XRT1 : TASK1 RENAMES REC.RT1;
          XAI1 : ARR_INT RENAMES AI1(1..3);
          XAA1 : ARR_ARR RENAMES AA1(2..4);
          XAR1 : ARR_REC RENAMES AR1(3..5);
          XAP1 : ARR_PTR RENAMES AP1(4..6);
          XAV1 : ARR_PVT RENAMES AV1(5..7);
          XAT1 : ARR_TSK RENAMES AT1(6..8);

          TASK TYPE TASK2 IS
               ENTRY ENTRY1 (TRI1 : OUT INTEGER; TRA1 : OUT ARRAY1;
                             TRR1 : OUT RECORD1; TRP1 : IN OUT POINTER1;
                             TRV1 : IN OUT PACK1.PRIVY;
                             TRT1 : IN OUT TASK1;
                             TAI1 : OUT ARR_INT; TAA1 : OUT ARR_ARR;
                             TAR1 : OUT ARR_REC; TAP1 : IN OUT ARR_PTR;
                             TAV1 : IN OUT ARR_PVT;
                             TAT1 : IN OUT ARR_TSK);
          END TASK2;

          CHK_TASK : TASK2;
          I : INTEGER;

          GENERIC
               GRI1 : IN OUT INTEGER;
               GRA1 : IN OUT ARRAY1;
               GRR1 : IN OUT RECORD1;
               GRP1 : IN OUT POINTER1;
               GRV1 : IN OUT PACK1.PRIVY;
               GRT1 : IN OUT TASK1;
               GAI1 : IN OUT ARR_INT;
               GAA1 : IN OUT ARR_ARR;
               GAR1 : IN OUT ARR_REC;
               GAP1 : IN OUT ARR_PTR;
               GAV1 : IN OUT ARR_PVT;
               GAT1 : IN OUT ARR_TSK;
          PACKAGE GENERIC2 IS
          END GENERIC2;

          PACKAGE BODY GENERIC2 IS
          BEGIN
               GRI1 := GRI1 + 1;
               GRA1 := (GRA1(1)+1, GRA1(2)+1, GRA1(3)+1);
               GRR1 := (D => 1, FIELD1 => GRR1.FIELD1+1);
               GRP1 := NEW INTEGER'(GRP1.ALL + 1);
               GRV1 := PACK1.NEXT(GRV1);
               GRT1.NEXT;
               GAI1 := (OTHERS => GAI1(GAI1'FIRST) + 1);
               GAA1 := (OTHERS => (OTHERS => GAA1(GAA1'FIRST)(1) + 1));
               GAR1 := (OTHERS => (D => 1,
                              FIELD1 => (GAR1(GAR1'FIRST).FIELD1 + 1)));
               GAP1 := (OTHERS =>
                         NEW INTEGER'(GAP1(GAP1'FIRST).ALL + 1));
               FOR J IN GAV1'RANGE LOOP
                    GAV1(J) := PACK1.NEXT(GAV1(J));
               END LOOP;
               FOR J IN GAT1'RANGE LOOP
                    GAT1(J).NEXT;
               END LOOP;
          END GENERIC2;

          PROCEDURE PROC1 (PRI1 : IN OUT INTEGER; PRA1 : IN OUT ARRAY1;
                           PRR1 : IN OUT RECORD1; PRP1 : OUT POINTER1;
                           PRV1 : OUT PACK1.PRIVY; PRT1 : IN OUT TASK1;
                           PAI1 : IN OUT ARR_INT; PAA1 : IN OUT ARR_ARR;
                           PAR1 : IN OUT ARR_REC; PAP1 : OUT ARR_PTR;
                           PAV1 : OUT ARR_PVT; PAT1 : IN OUT ARR_TSK) IS
          BEGIN
               PRI1 := PRI1 + 1;
               PRA1 := (PRA1(1)+1, PRA1(2)+1, PRA1(3)+1);
               PRR1 := (D => 1, FIELD1 => PRR1.FIELD1 + 1);
               PRP1 := NEW INTEGER'(REC.RP1.ALL + 1);
               PRV1 := PACK1.NEXT(REC.RV1);
               PRT1.NEXT;
               PAI1 := (OTHERS => PAI1(PAI1'FIRST) + 1);
               PAA1 := (OTHERS => (OTHERS => PAA1(PAA1'FIRST)(1) + 1));
               PAR1 := (OTHERS => (D => 1, FIELD1 =>
                                   (PAR1(PAR1'FIRST).FIELD1 + 1)));
               PAP1 := (OTHERS =>
                        NEW INTEGER'(AP1(PAP1'FIRST).ALL + 1));
               FOR J IN PAV1'RANGE LOOP
                    PAV1(J) := PACK1.NEXT(AV1(J));
               END LOOP;
               FOR J IN PAT1'RANGE LOOP
                    PAT1(J).NEXT;
               END LOOP;
          END PROC1;

          TASK BODY TASK2 IS
          BEGIN
               ACCEPT ENTRY1 (TRI1 : OUT INTEGER; TRA1 : OUT ARRAY1;
                              TRR1 : OUT RECORD1;
                              TRP1 : IN OUT POINTER1;
                              TRV1 : IN OUT PACK1.PRIVY;
                              TRT1: IN OUT TASK1;
                              TAI1 : OUT ARR_INT; TAA1 : OUT ARR_ARR;
                              TAR1 : OUT ARR_REC; TAP1 : IN OUT ARR_PTR;
                              TAV1 : IN OUT ARR_PVT;
                              TAT1 : IN OUT ARR_TSK)
               DO
                    TRI1 := REC.RI1 + 1;
                    TRA1 := (REC.RA1(1)+1, REC.RA1(2)+1, REC.RA1(3)+1);
                    TRR1 := (D => 1, FIELD1 => REC.RR1.FIELD1 + 1);
                    TRP1 := NEW INTEGER'(TRP1.ALL + 1);
                    TRV1 := PACK1.NEXT(TRV1);
                    TRT1.NEXT;
                    TAI1 := (OTHERS => AI1(TAI1'FIRST) + 1);
                    TAA1 := (OTHERS => (OTHERS =>
                                        AA1(TAA1'FIRST)(1) + 1));
                    TAR1 := (OTHERS => (D => 1, FIELD1 =>
                                        (AR1(TAR1'FIRST).FIELD1 + 1)));
                    TAP1 := (OTHERS =>
                             NEW INTEGER'(TAP1(TAP1'FIRST).ALL + 1));
                    FOR J IN TAV1'RANGE LOOP
                         TAV1(J) := PACK1.NEXT(TAV1(J));
                    END LOOP;
                    FOR J IN TAT1'RANGE LOOP
                         TAT1(J).NEXT;
                    END LOOP;
               END ENTRY1;
          END TASK2;

          PACKAGE GENPACK2 IS NEW
               GENERIC2 (XRI1, XRA1, XRR1, XRP1, XRV1, XRT1,
                         XAI1, XAA1, XAR1, XAP1, XAV1, XAT1);

     BEGIN
          IF XRI1 /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XRI1 (1)");
          END IF;

          IF XRA1 /= (IDENT_INT(1),IDENT_INT(1),IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XRA1 (1)");
          END IF;

          IF XRR1 /= (D => 1, FIELD1 => IDENT_INT(1)) THEN
               FAILED ("INCORRECT VALUE OF XRR1 (1)");
          END IF;

          IF XRP1 /= IDENT(REC.RP1) OR XRP1.ALL /= IDENT_INT(1) THEN
               FAILED ("INCORRECT VALUE OF XRP1 (1)");
          END IF;

          IF PACK1."/=" (XRV1, PACK1.IDENT(PACK1.ONE)) THEN
               FAILED ("INCORRECT VALUE OF XRV1 (1)");
          END IF;

          XRT1.VALU(I);
          IF I /= IDENT_INT(1) THEN
               FAILED ("INCORRECT RETURN VALUE OF XRT1.VALU (1)");
          END IF;

          FOR J IN XAI1'RANGE LOOP
               IF XAI1(J) /= IDENT_INT(1) THEN
                    FAILED ("INCORRECT VALUE OF XAI1(" &
                            INTEGER'IMAGE(J) & ") (1)");
               END IF;
          END LOOP;

          FOR J IN XAA1'RANGE LOOP
               IF XAA1(J) /= (IDENT_INT(1),IDENT_INT(1),IDENT_INT(1))
               THEN
                    FAILED ("INCORRECT VALUE OF XAA1(" &
                            INTEGER'IMAGE(J) & ") (1)");
               END IF;
          END LOOP;

          FOR J IN XAR1'RANGE LOOP
               IF XAR1(J) /= (D => 1, FIELD1 => IDENT_INT(1)) THEN
                    FAILED ("INCORRECT VALUE OF XAR1(" &
                            INTEGER'IMAGE(J) & ") (1)");
               END IF;
          END LOOP;

          FOR J IN XAP1'RANGE LOOP
               IF XAP1(J) /= IDENT(AP1(J)) OR
                  XAP1(J).ALL /= IDENT_INT(1)
               THEN
                    FAILED ("INCORRECT VALUE OF XAP1(" &
                            INTEGER'IMAGE(J) & ") (1)");
               END IF;
          END LOOP;

          FOR J IN XAV1'RANGE LOOP
               IF PACK1."/=" (XAV1(J), PACK1.IDENT(PACK1.ONE)) THEN
                    FAILED ("INCORRECT VALUE OF XAV1(" &
                            INTEGER'IMAGE(J) & ") (1)");
               END IF;
          END LOOP;

          FOR J IN XAT1'RANGE LOOP
               XAT1(J).VALU(I);
               IF I /= IDENT_INT(1) THEN
                    FAILED ("INCORRECT RETURN VALUE FROM XAT1(" &
                            INTEGER'IMAGE(J) & ").VALU (1)");
               END IF;
          END LOOP;

          PROC1 (XRI1, XRA1, XRR1, XRP1, XRV1, XRT1,
                 XAI1, XAA1, XAR1, XAP1, XAV1, XAT1);

          IF XRI1 /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XRI1 (2)");
          END IF;

          IF XRA1 /= (IDENT_INT(2),IDENT_INT(2),IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XRA1 (2)");
          END IF;

          IF XRR1 /= (D => 1, FIELD1 => IDENT_INT(2)) THEN
               FAILED ("INCORRECT VALUE OF XRR1 (2)");
          END IF;

          IF XRP1 /= IDENT(REC.RP1) OR XRP1.ALL /= IDENT_INT(2) THEN
               FAILED ("INCORRECT VALUE OF XRP1 (2)");
          END IF;

          IF PACK1."/=" (XRV1, PACK1.IDENT(PACK1.TWO)) THEN
               FAILED ("INCORRECT VALUE OF XRV1 (2)");
          END IF;

          XRT1.VALU(I);
          IF I /= IDENT_INT(2) THEN
               FAILED ("INCORRECT RETURN VALUE FROM XRT1.VALU (2)");
          END IF;

          FOR J IN XAI1'RANGE LOOP
               IF XAI1(J) /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE OF XAI1(" &
                            INTEGER'IMAGE(J) & ") (2)");
               END IF;
          END LOOP;

          FOR J IN XAA1'RANGE LOOP
               IF XAA1(J) /= (IDENT_INT(2),IDENT_INT(2),IDENT_INT(2))
               THEN
                    FAILED ("INCORRECT VALUE OF XAA1(" &
                            INTEGER'IMAGE(J) & ") (2)");
               END IF;
          END LOOP;

          FOR J IN XAR1'RANGE LOOP
               IF XAR1(J) /= (D => 1, FIELD1 => IDENT_INT(2)) THEN
                    FAILED ("INCORRECT VALUE OF XAR1(" &
                            INTEGER'IMAGE(J) & ") (2)");
               END IF;
          END LOOP;

          FOR J IN XAP1'RANGE LOOP
               IF XAP1(J) /= IDENT(AP1(J)) OR
                  XAP1(J).ALL /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT VALUE OF XAP1(" &
                            INTEGER'IMAGE(J) & ") (2)");
               END IF;
          END LOOP;

          FOR J IN XAV1'RANGE LOOP
               IF PACK1."/=" (XAV1(J), PACK1.IDENT(PACK1.TWO)) THEN
                    FAILED ("INCORRECT VALUE OF XAV1(" &
                            INTEGER'IMAGE(J) & ") (2)");
               END IF;
          END LOOP;

          FOR J IN XAT1'RANGE LOOP
               XAT1(J).VALU(I);
               IF I /= IDENT_INT(2) THEN
                    FAILED ("INCORRECT RETURN VALUE FROM XAT1(" &
                            INTEGER'IMAGE(J) & ").VALU (2)");
               END IF;
          END LOOP;

          CHK_TASK.ENTRY1(XRI1, XRA1, XRR1, XRP1, XRV1, XRT1,
                          XAI1, XAA1, XAR1, XAP1, XAV1, XAT1);

          IF XRI1 /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XRI1 (3)");
          END IF;

          IF XRA1 /= (IDENT_INT(3),IDENT_INT(3),IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XRA1 (3)");
          END IF;

          IF XRR1 /= (D => 1, FIELD1 => IDENT_INT(3)) THEN
               FAILED ("INCORRECT VALUE OF XRR1 (3)");
          END IF;

          IF XRP1 /= IDENT(REC.RP1) OR XRP1.ALL /= IDENT_INT(3) THEN
               FAILED ("INCORRECT VALUE OF XRP1 (3)");
          END IF;

          IF PACK1."/=" (XRV1, PACK1.IDENT(PACK1.THREE)) THEN
               FAILED ("INCORRECT VALUE OF XRV1 (3)");
          END IF;

          XRT1.VALU(I);
          IF I /= IDENT_INT(3) THEN
               FAILED ("INCORRECT RETURN VALUE OF XRT1.VALU (3)");
          END IF;

          FOR J IN XAI1'RANGE LOOP
               IF XAI1(J) /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE OF XAI1(" &
                            INTEGER'IMAGE(J) & ") (3)");
               END IF;
          END LOOP;

          FOR J IN XAA1'RANGE LOOP
               IF XAA1(J) /= (IDENT_INT(3),IDENT_INT(3),IDENT_INT(3))
               THEN
                    FAILED ("INCORRECT VALUE OF XAA1(" &
                            INTEGER'IMAGE(J) & ") (3)");
               END IF;
          END LOOP;

          FOR J IN XAR1'RANGE LOOP
               IF XAR1(J) /= (D => 1, FIELD1 => IDENT_INT(3)) THEN
                    FAILED ("INCORRECT VALUE OF XAR1(" &
                            INTEGER'IMAGE(J) & ") (3)");
               END IF;
          END LOOP;

          FOR J IN XAP1'RANGE LOOP
               IF XAP1(J) /= IDENT(AP1(J)) OR
                  XAP1(J).ALL /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT VALUE OF XAP1(" &
                            INTEGER'IMAGE(J) & ") (3)");
               END IF;
          END LOOP;

          FOR J IN XAV1'RANGE LOOP
               IF PACK1."/=" (XAV1(J), PACK1.IDENT(PACK1.THREE)) THEN
                    FAILED ("INCORRECT VALUE OF XAV1(" &
                            INTEGER'IMAGE(J) & ") (3)");
               END IF;
          END LOOP;

          FOR J IN XAT1'RANGE LOOP
               XAT1(J).VALU(I);
               IF I /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT RETURN VALUE FROM XAT1(" &
                            INTEGER'IMAGE(J) & ").VALU (3)");
               END IF;
          END LOOP;

          XRI1 := XRI1 + 1;
          XRA1 := (XRA1(1)+1, XRA1(2)+1, XRA1(3)+1);
          XRR1 := (D => 1, FIELD1 => XRR1.FIELD1 + 1);
          XRP1 := NEW INTEGER'(XRP1.ALL + 1);
          XRV1 := PACK1.NEXT(XRV1);
          XRT1.NEXT;
          XAI1 := (OTHERS => XAI1(XAI1'FIRST) + 1);
          XAA1 := (OTHERS => (OTHERS => XAA1(XAA1'FIRST)(1) + 1));
          XAR1 := (OTHERS => (D => 1,
                         FIELD1 => (XAR1(XAR1'FIRST).FIELD1 + 1)));
          XAP1 := (OTHERS => NEW INTEGER'(XAP1(XAP1'FIRST).ALL + 1));
          FOR J IN XAV1'RANGE LOOP
               XAV1(J) := PACK1.NEXT(XAV1(J));
          END LOOP;
          FOR J IN XAT1'RANGE LOOP
               XAT1(J).NEXT;
          END LOOP;

          IF XRI1 /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XRI1 (4)");
          END IF;

          IF XRA1 /= (IDENT_INT(4),IDENT_INT(4),IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XRA1 (4)");
          END IF;

          IF XRR1 /= (D => 1, FIELD1 => IDENT_INT(4)) THEN
               FAILED ("INCORRECT VALUE OF XRR1 (4)");
          END IF;

          IF XRP1 /= IDENT(REC.RP1) OR XRP1.ALL /= IDENT_INT(4) THEN
               FAILED ("INCORRECT VALUE OF XRP1 (4)");
          END IF;

          IF PACK1."/=" (XRV1, PACK1.IDENT(PACK1.FOUR)) THEN
               FAILED ("INCORRECT VALUE OF XRV1 (4)");
          END IF;

          XRT1.VALU(I);
          IF I /= IDENT_INT(4) THEN
               FAILED ("INCORRECT RETURN VALUE OF XRT1.VALU (4)");
          END IF;

          FOR J IN XAI1'RANGE LOOP
               IF XAI1(J) /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT VALUE OF XAI1(" &
                            INTEGER'IMAGE(J) & ") (4)");
               END IF;
          END LOOP;

          FOR J IN XAA1'RANGE LOOP
               IF XAA1(J) /= (IDENT_INT(4),IDENT_INT(4),IDENT_INT(4))
               THEN
                    FAILED ("INCORRECT VALUE OF XAA1(" &
                            INTEGER'IMAGE(J) & ") (4)");
               END IF;
          END LOOP;

          FOR J IN XAR1'RANGE LOOP
               IF XAR1(J) /= (D => 1, FIELD1 => IDENT_INT(4)) THEN
                    FAILED ("INCORRECT VALUE OF XAR1(" &
                            INTEGER'IMAGE(J) & ") (4)");
               END IF;
          END LOOP;

          FOR J IN XAP1'RANGE LOOP
               IF XAP1(J) /= IDENT(AP1(J)) OR
                  XAP1(J).ALL /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT VALUE OF XAP1(" &
                            INTEGER'IMAGE(J) & ") (4)");
               END IF;
          END LOOP;

          FOR J IN XAV1'RANGE LOOP
               IF PACK1."/=" (XAV1(J), PACK1.IDENT(PACK1.FOUR)) THEN
                    FAILED ("INCORRECT VALUE OF XAV1(" &
                            INTEGER'IMAGE(J) & ") (4)");
               END IF;
          END LOOP;

          FOR J IN XAT1'RANGE LOOP
               XAT1(J).VALU(I);
               IF I /= IDENT_INT(4) THEN
                    FAILED ("INCORRECT RETURN VALUE FROM XAT1(" &
                            INTEGER'IMAGE(J) & ").VALU (4)");
               END IF;
          END LOOP;

          REC.RI1 := REC.RI1 + 1;
          REC.RA1 := (REC.RA1(1)+1, REC.RA1(2)+1, REC.RA1(3)+1);
          REC.RR1 := (D => 1, FIELD1 => REC.RR1.FIELD1 + 1);
          REC.RP1 := NEW INTEGER'(REC.RP1.ALL + 1);
          REC.RV1 := PACK1.NEXT(REC.RV1);
          REC.RT1.NEXT;
          AI1 := (OTHERS => AI1(XAI1'FIRST) + 1);
          AA1 := (OTHERS => (OTHERS => AA1(XAA1'FIRST)(1) + 1));
          AR1 := (OTHERS => (D => 1,
                             FIELD1 => (AR1(XAR1'FIRST).FIELD1 + 1)));
          AP1 := (OTHERS => NEW INTEGER'(AP1(XAP1'FIRST).ALL + 1));
          FOR J IN XAV1'RANGE LOOP
               AV1(J) := PACK1.NEXT(AV1(J));
          END LOOP;
          FOR J IN XAT1'RANGE LOOP
               AT1(J).NEXT;
          END LOOP;

          IF XRI1 /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XRI1 (5)");
          END IF;

          IF XRA1 /= (IDENT_INT(5),IDENT_INT(5),IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XRA1 (5)");
          END IF;

          IF XRR1 /= (D => 1, FIELD1 => IDENT_INT(5)) THEN
               FAILED ("INCORRECT VALUE OF XRR1 (5)");
          END IF;

          IF XRP1 /= IDENT(REC.RP1) OR XRP1.ALL /= IDENT_INT(5) THEN
               FAILED ("INCORRECT VALUE OF XRP1 (5)");
          END IF;

          IF PACK1."/=" (XRV1, PACK1.IDENT(PACK1.FIVE)) THEN
               FAILED ("INCORRECT VALUE OF XRV1 (5)");
          END IF;

          XRT1.VALU(I);
          IF I /= IDENT_INT(5) THEN
               FAILED ("INCORRECT RETURN VALUE OF XRT1.VALU (5)");
          END IF;

          FOR J IN XAI1'RANGE LOOP
               IF XAI1(J) /= IDENT_INT(5) THEN
                    FAILED ("INCORRECT VALUE OF XAI1(" &
                            INTEGER'IMAGE(J) & ") (5)");
               END IF;
          END LOOP;

          FOR J IN XAA1'RANGE LOOP
               IF XAA1(J) /= (IDENT_INT(5),IDENT_INT(5),IDENT_INT(5))
               THEN
                    FAILED ("INCORRECT VALUE OF XAA1(" &
                            INTEGER'IMAGE(J) & ") (5)");
               END IF;
          END LOOP;

          FOR J IN XAR1'RANGE LOOP
               IF XAR1(J) /= (D => 1, FIELD1 => IDENT_INT(5)) THEN
                    FAILED ("INCORRECT VALUE OF XAR1(" &
                            INTEGER'IMAGE(J) & ") (5)");
               END IF;
          END LOOP;

          FOR J IN XAP1'RANGE LOOP
               IF XAP1(J) /= IDENT(AP1(J)) OR
               XAP1(J).ALL /= IDENT_INT(5) THEN
                    FAILED ("INCORRECT VALUE OF XAP1(" &
                            INTEGER'IMAGE(J) & ") (5)");
               END IF;
          END LOOP;

          FOR J IN XAV1'RANGE LOOP
               IF PACK1."/=" (XAV1(J), PACK1.IDENT(PACK1.FIVE)) THEN
                    FAILED ("INCORRECT VALUE OF XAV1(" &
                            INTEGER'IMAGE(J) & ") (5)");
               END IF;
          END LOOP;

          FOR J IN XAT1'RANGE LOOP
               XAT1(J).VALU(I);
               IF I /= IDENT_INT(5) THEN
                    FAILED ("INCORRECT RETURN VALUE FROM XAT1(" &
                            INTEGER'IMAGE(J) & ").VALU (5)");
               END IF;
          END LOOP;
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
     TEST ("C85006D", "CHECK THAT A COMPONENT OR SLICE OF A VARIABLE " &
                      "CREATED BY A GENERIC 'IN OUT' FORMAL " &
                      "PARAMETER CAN BE RENAMED AND HAS THE CORRECT " &
                      "VALUE, AND THAT THE NEW NAME CAN BE USED IN " &
                      "AN ASSIGNMENT STATEMENT AND PASSED ON AS AN " &
                      "ACTUAL SUBPROGRAM OR ENTRY 'IN OUT' OR 'OUT' " &
                      "PARAMETER, AND AS AN ACTUAL GENERIC 'IN OUT' " &
                      "PARAMETER, AND THAT WHEN THE VALUE OF THE " &
                      "RENAMED VARIABLE IS CHANGED, THE NEW VALUE IS " &
                      "REFLECTED BY THE VALUE OF THE NEW NAME");

     DECLARE
          PACKAGE GENPACK IS NEW
               GENERIC1 (DREC, DAI1, DAA1, DAR1, DAP1, DAV1, DAT1);
     BEGIN
          NULL;
     END;

     DREC.RT1.STOP;

     FOR I IN DAT1'RANGE LOOP
          DAT1(I).STOP;
     END LOOP;

     RESULT;
END C85006D;
