-- C38002A.ADA

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
--     CHECK THAT AN UNCONSTRAINED ARRAY TYPE OR A RECORD WITHOUT
--     DEFAULT DISCRIMINANTS CAN BE USED IN AN ACCESS_TYPE_DEFINITION
--     WITHOUT AN INDEX OR DISCRIMINANT CONSTRAINT.
--
--     CHECK THAT (NON-STATIC) INDEX OR DISCRIMINANT CONSTRAINTS CAN
--     SUBSEQUENTLY BE IMPOSED WHEN THE TYPE IS USED IN AN OBJECT
--     DECLARATION, ARRAY COMPONENT DECLARATION, RECORD COMPONENT
--     DECLARATION, ACCESS TYPE DECLARATION, PARAMETER DECLARATION,
--     DERIVED TYPE DEFINITION, PRIVATE TYPE.
--
--     CHECK FOR UNCONSTRAINED GENERIC FORMAL TYPE.

-- HISTORY:
--     AH  09/02/86 CREATED ORIGINAL TEST.
--     DHH 08/16/88 REVISED HEADER AND ENTERED COMMENTS FOR PRIVATE TYPE
--                  AND CORRECTED INDENTATION.
--     BCB 04/12/90 ADDED CHECKS FOR AN ARRAY AS A SUBPROGRAM RETURN
--                  TYPE AND AN ARRAY AS A FORMAL PARAMETER.
--     LDC 10/01/90 ADDED CODE SO F, FPROC, G, GPROC AREN'T OPTIMIZED 
--                  AWAY

WITH REPORT; USE REPORT;
PROCEDURE C38002A IS

BEGIN
     TEST ("C38002A", "NON-STATIC CONSTRAINTS CAN BE IMPOSED " &
           "ON ACCESS TYPES ACCESSING PREVIOUSLY UNCONSTRAINED " &
           "ARRAY OR RECORD TYPES");

     DECLARE
          C3 : CONSTANT INTEGER := IDENT_INT(3);

          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          TYPE ARR_NAME IS ACCESS ARR;
          SUBTYPE ARR_NAME_3 IS ARR_NAME(1..3);

          TYPE REC(DISC : INTEGER) IS
               RECORD
                    COMP : ARR_NAME(1..DISC);
               END RECORD;
          TYPE REC_NAME IS ACCESS REC;

          OBJ : REC_NAME(C3);

          TYPE ARR2 IS ARRAY (1..10) OF REC_NAME(C3);

          TYPE REC2 IS
               RECORD
                    COMP2 : REC_NAME(C3);
               END RECORD;

          TYPE NAME_REC_NAME IS ACCESS REC_NAME(C3);

          TYPE DERIV IS NEW REC_NAME(C3);
          SUBTYPE REC_NAME_3 IS REC_NAME(C3);

          FUNCTION F (PARM : REC_NAME_3) RETURN REC_NAME_3 IS
          BEGIN
               IF NOT EQUAL(IDENT_INT(3), 1 + IDENT_INT(2)) THEN
                    COMMENT("DON'T OPTIMIZE F AWAY");
               END IF;
               RETURN PARM;
          END;

          PROCEDURE FPROC (PARM : REC_NAME_3) IS
          BEGIN
               IF NOT EQUAL(IDENT_INT(4), 2 + IDENT_INT(2)) THEN
                    COMMENT("DON'T OPTIMIZE FPROC AWAY");
               END IF;
          END FPROC;

          FUNCTION G (PA : ARR_NAME_3) RETURN ARR_NAME_3 IS
          BEGIN
               IF NOT EQUAL(IDENT_INT(5), 3 + IDENT_INT(2)) THEN
                    COMMENT("DON'T OPTIMIZE G AWAY");
               END IF;
               RETURN PA;
          END G;

          PROCEDURE GPROC (PA : ARR_NAME_3) IS
          BEGIN
               IF NOT EQUAL(IDENT_INT(6), 4 + IDENT_INT(2)) THEN
                    COMMENT("DON'T OPTIMIZE GPROC AWAY");
               END IF;
          END GPROC;

     BEGIN
          DECLARE
               R : REC_NAME;
          BEGIN
               R := NEW REC'(DISC => 3, COMP => NEW ARR'(1..3 => 5));
               R := F(R);
               R := NEW REC'(DISC => 4, COMP => NEW ARR'(1..4 => 5));
               R := F(R);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY FUNCTION FOR RECORD");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF R = NULL OR ELSE R.DISC /= 4 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                                 "ACCESS VALUE - RECORD,FUNCTION");
                    END IF;
          END;

          DECLARE
               R : REC_NAME;
          BEGIN
               R := NEW REC'(DISC => 3, COMP => NEW ARR'(1..3 => 5));
               FPROC(R);
               R := NEW REC'(DISC => 4, COMP => NEW ARR'(1..4 => 5));
               FPROC(R);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY PROCEDURE FOR RECORD");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF R = NULL OR ELSE R.DISC /= 4 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                                 "ACCESS VALUE - RECORD,PROCEDURE");
                    END IF;
          END;

          DECLARE
               A : ARR_NAME;
          BEGIN
               A := NEW ARR'(1..3 => 5);
               A := G(A);
               A := NEW ARR'(1..4 => 6);
               A := G(A);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY FUNCTION FOR ARRAY");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF A = NULL OR ELSE A(4) /= 6 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                                 "ACCESS VALUE - ARRAY,FUNCTION");
                    END IF;
          END;

          DECLARE
               A : ARR_NAME;
          BEGIN
               A := NEW ARR'(1..3 => 5);
               GPROC(A);
               A := NEW ARR'(1..4 => 6);
               GPROC(A);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY PROCEDURE FOR ARRAY");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF A = NULL OR ELSE A(4) /= 6 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT OF " &
                                 "ACCESS VALUE - ARRAY,PROCEDURE");
                    END IF;
          END;
     END;

     DECLARE
          C3 : CONSTANT INTEGER := IDENT_INT(3);

          TYPE REC (DISC : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE P_ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          TYPE P_ARR_NAME IS ACCESS P_ARR;

          TYPE P_REC_NAME IS ACCESS REC;

          GENERIC
               TYPE UNCON_ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
          PACKAGE P IS
               TYPE ACC_REC IS ACCESS REC;
               TYPE ACC_ARR IS ACCESS UNCON_ARR;
               TYPE ACC_P_ARR IS ACCESS P_ARR;
               SUBTYPE ACC_P_ARR_3 IS ACC_P_ARR(1..3);
               OBJ : ACC_REC(C3);

               TYPE ARR2 IS ARRAY (1..10) OF ACC_REC(C3);

               TYPE REC1 IS
                    RECORD
                         COMP1 : ACC_REC(C3);
                    END RECORD;

               TYPE REC2 IS
                    RECORD
                         COMP2 : ACC_ARR(1..C3);
                    END RECORD;

               SUBTYPE ACC_REC_3 IS ACC_REC(C3);

               FUNCTION F (PARM : ACC_REC_3) RETURN ACC_REC_3;

               PROCEDURE FPROC (PARM : ACC_REC_3);

               FUNCTION G (PA : ACC_P_ARR_3) RETURN ACC_P_ARR_3;

               PROCEDURE GPROC (PA : ACC_P_ARR_3);

               TYPE ACC1 IS PRIVATE;
               TYPE ACC2 IS PRIVATE;
               TYPE DER1 IS PRIVATE;
               TYPE DER2 IS PRIVATE;

          PRIVATE

               TYPE ACC1 IS ACCESS ACC_REC(C3);
               TYPE ACC2 IS ACCESS ACC_ARR(1..C3);
               TYPE DER1 IS NEW ACC_REC(C3);
               TYPE DER2 IS NEW ACC_ARR(1..C3);
          END P;

          PACKAGE BODY P IS
               FUNCTION F (PARM : ACC_REC_3) RETURN ACC_REC_3 IS
               BEGIN
                    IF NOT EQUAL(IDENT_INT(3), 1 + IDENT_INT(2)) THEN
                         COMMENT("DON'T OPTIMIZE F AWAY");
                    END IF;
                    RETURN PARM;
               END;

               PROCEDURE FPROC (PARM : ACC_REC_3) IS
               BEGIN
                    IF NOT EQUAL(IDENT_INT(4), 2 + IDENT_INT(2)) THEN
                         COMMENT("DON'T OPTIMIZE FPROC AWAY");
                    END IF;
               END FPROC;

               FUNCTION G (PA : ACC_P_ARR_3) RETURN ACC_P_ARR_3 IS
               BEGIN
                    IF NOT EQUAL(IDENT_INT(5), 3 + IDENT_INT(2)) THEN
                         COMMENT("DON'T OPTIMIZE G AWAY");
                    END IF;
                    RETURN PA;
               END;

               PROCEDURE GPROC (PA : ACC_P_ARR_3) IS
               BEGIN
                    IF NOT EQUAL(IDENT_INT(6), 4 + IDENT_INT(2)) THEN
                         COMMENT("DON'T OPTIMIZE GPROC AWAY");
                    END IF;
               END GPROC;
          END P;

          PACKAGE NP IS NEW P (UNCON_ARR => P_ARR);

          USE NP;

     BEGIN
          DECLARE
               R : ACC_REC;
          BEGIN
               R := NEW REC(DISC => 3);
               R := F(R);
               R := NEW REC(DISC => 4);
               R := F(R);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY FUNCTION FOR A RECORD -GENERIC");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF R = NULL OR ELSE R.DISC /= 4 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT " &
                                 "OF ACCESS VALUE - RECORD," &
                                 "FUNCTION -GENERIC");
                    END IF;
          END;

          DECLARE
               R : ACC_REC;
          BEGIN
               R := NEW REC(DISC => 3);
               FPROC(R);
               R := NEW REC(DISC => 4);
               FPROC(R);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY PROCEDURE FOR A RECORD -GENERIC");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF R = NULL OR ELSE R.DISC /= 4 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT " &
                                 "OF ACCESS VALUE - RECORD," &
                                 "PROCEDURE -GENERIC");
                    END IF;
          END;

          DECLARE
               A : ACC_P_ARR;
          BEGIN
               A := NEW P_ARR'(1..3 => 5);
               A := G(A);
               A := NEW P_ARR'(1..4 => 6);
               A := G(A);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY FUNCTION FOR AN ARRAY -GENERIC");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF A = NULL OR ELSE A(4) /= 6 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT " &
                                 "OF ACCESS VALUE - ARRAY," &
                                 "FUNCTION -GENERIC");
                    END IF;
          END;

          DECLARE
               A : ACC_P_ARR;
          BEGIN
               A := NEW P_ARR'(1..3 => 5);
               GPROC(A);
               A := NEW P_ARR'(1..4 => 6);
               GPROC(A);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY PROCEDURE FOR AN ARRAY -GENERIC");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF A = NULL OR ELSE A(4) /= 6 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT " &
                                 "OF ACCESS VALUE - ARRAY," &
                                 "PROCEDURE -GENERIC");
                    END IF;
          END;
     END;

     DECLARE
          TYPE CON_INT IS RANGE 1..10;

          GENERIC
               TYPE UNCON_INT IS RANGE <>;
          PACKAGE P2 IS
               SUBTYPE NEW_INT IS UNCON_INT RANGE 1..5;
               FUNCTION FUNC_INT (PARM : NEW_INT) RETURN NEW_INT;

               PROCEDURE PROC_INT (PARM : NEW_INT);
          END P2;

          PACKAGE BODY P2 IS
               FUNCTION FUNC_INT (PARM : NEW_INT) RETURN NEW_INT IS
               BEGIN
                    IF NOT EQUAL(IDENT_INT(3), 1 + IDENT_INT(2)) THEN
                         COMMENT("DON'T OPTIMIZE F AWAY");
                    END IF;
                    RETURN PARM;
               END FUNC_INT;

               PROCEDURE PROC_INT (PARM : NEW_INT) IS
               BEGIN
                    IF NOT EQUAL(IDENT_INT(4), 2 + IDENT_INT(2)) THEN
                         COMMENT("DON'T OPTIMIZE FPROC AWAY");
                    END IF;
               END PROC_INT;
          END P2;

          PACKAGE NP2 IS NEW P2 (UNCON_INT => CON_INT);

          USE NP2;

     BEGIN
          DECLARE
               R : CON_INT;
          BEGIN
               R := 2;
               R := FUNC_INT(R);
               R := 8;
               R := FUNC_INT(R);
               FAILED ("INCOMPATIBLE CONSTRAINT ON VALUE " &
                       "ACCEPTED BY FUNCTION -GENERIC");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF R /= 8 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT " &
                                 "OF VALUE -FUNCTION, GENERIC");
                    END IF;
          END;

          DECLARE
               R : CON_INT;
          BEGIN
               R := 2;
               PROC_INT(R);
               R := 9;
               PROC_INT(R);
               FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE " &
                       "ACCEPTED BY PROCEDURE -GENERIC");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    IF R /= 9 THEN
                         FAILED ("ERROR IN EVALUATION/ASSIGNMENT " &
                                 "OF ACCESS VALUE - PROCEDURE, " &
                                 "GENERIC");
                    END IF;
          END;
     END;

     RESULT;
END C38002A;
