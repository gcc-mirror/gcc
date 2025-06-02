-- CC1225A.TST

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
--     CHECK, FOR A FORMAL ACCESS TYPE, THAT ALL ALLOWABLE OPERATIONS
--     ARE IMPLICITLY DECLARED.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     BCB 03/29/88  CREATED ORIGINAL TEST.
--     RDH 04/09/90  ADDED 'STORAGE_SIZE CLAUSES.  CHANGED EXTENSION TO
--                   'TST'.
--     LDC 09/26/90  REMOVED 'USE PACK' AFTER THE WITH SINCE IT ISN'T 
--                   NEEDED, ADDED CHECK FOR NULL AFTER ASSIGMENT TO
--                   NULL, ADDED CHECKS FOR OTHER RELATION OPERATORS,
--                   CHANGED CHECK FOR 'ADDRESS TO A PROCEDURE CALL.
--     LDC 10/13/90  CHANGED CHECK FOR 'SIZE TO ONLY CHECK FOR 
--                   AVAILABILITY.  CHANGED CHECK FOR 'ADDRESS TO A 
--                   MEMBERSHIP TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE CC1225A IS

     TASK_STORAGE_SIZE : CONSTANT := $TASK_STORAGE_SIZE;

     TYPE AI IS ACCESS INTEGER;

     TYPE ACCINTEGER IS ACCESS INTEGER;

     TYPE REC IS RECORD
          COMP : INTEGER;
     END RECORD;

     TYPE DISCREC (DISC : INTEGER := 1) IS RECORD
          COMPD : INTEGER;
     END RECORD;

     TYPE AREC IS ACCESS REC;

     TYPE ADISCREC IS ACCESS DISCREC;

     TYPE ARR IS ARRAY(1..2,1..2) OF INTEGER;

     TYPE ONEDIM IS ARRAY(1..10) OF INTEGER;

     TYPE AA IS ACCESS ARR;

     TYPE AONEDIM IS ACCESS ONEDIM;

     TYPE ENUM IS (ONE, TWO, THREE);

     TASK TYPE T IS
          ENTRY HERE(VAL : IN OUT INTEGER);
     END T;

     TYPE ATASK IS ACCESS T;

     TYPE ANOTHERTASK IS ACCESS T;
     FOR ANOTHERTASK'STORAGE_SIZE USE 2 * TASK_STORAGE_SIZE;

     TASK TYPE T1 IS
          ENTRY HERE1(ENUM)(VAL1 : IN OUT INTEGER);
     END T1;

     TYPE ATASK1 IS ACCESS T1;

     TASK BODY T IS
     BEGIN
          ACCEPT HERE(VAL : IN OUT INTEGER) DO
               VAL := VAL * 2;
          END HERE;
     END T;

     TASK BODY T1 IS
     BEGIN
          SELECT
               ACCEPT HERE1(ONE)(VAL1 : IN OUT INTEGER) DO
                    VAL1 := VAL1 * 1;
               END HERE1;
          OR
               ACCEPT HERE1(TWO)(VAL1 : IN OUT INTEGER) DO
                    VAL1 := VAL1 * 2;
               END HERE1;
          OR
               ACCEPT HERE1(THREE)(VAL1 : IN OUT INTEGER) DO
                    VAL1 := VAL1 * 3;
               END HERE1;
          END SELECT;
     END T1;

     GENERIC
          TYPE FORM IS (<>);
          TYPE ACCFORM IS ACCESS FORM;
          TYPE ACC IS ACCESS INTEGER;
          TYPE ACCREC IS ACCESS REC;
          TYPE ACCDISCREC IS ACCESS DISCREC;
          TYPE ACCARR IS ACCESS ARR;
          TYPE ACCONE IS ACCESS ONEDIM;
          TYPE ACCTASK IS ACCESS T;
          TYPE ACCTASK1 IS ACCESS T1;
          TYPE ANOTHERTASK1 IS ACCESS T;
     PACKAGE P IS
     END P;

     PACKAGE BODY P IS
          AF : ACCFORM;
          TYPE DER_ACC IS NEW ACC;
          A, B : ACC;
          DERA : DER_ACC;
          R : ACCREC;
          DR : ACCDISCREC;
          C : ACCARR;
          D, E : ACCONE;
          F : ACCTASK;
          G : ACCTASK1;
          INT : INTEGER := 5;

     BEGIN
          TEST ("CC1225A", "CHECK, FOR A FORMAL ACCESS TYPE, THAT " &
                           "ALL ALLOWABLE OPERATIONS ARE IMPLICITLY " &
                           "DECLARED");

          IF AF'ADDRESS NOT IN ADDRESS THEN
               FAILED ("IMPROPER RESULT FROM AF'ADDRESS TEST");
          END IF;

          DECLARE
               AF_SIZE : INTEGER := ACCFORM'SIZE;
          BEGIN
               IF AF_SIZE NOT IN INTEGER THEN
                    FAILED ("IMPROPER RESULT FROM AF'SIZE");
               END IF;
          END;

          IF ANOTHERTASK1'STORAGE_SIZE < TASK_STORAGE_SIZE THEN
               FAILED ("IMPROPER VALUE FOR ANOTHERTASK1'STORAGE_SIZE");
          END IF;

          B := NEW INTEGER'(25);

          A := B;

          IF A.ALL /= 25 THEN
               FAILED ("IMPROPER VALUE FOR ASSIGNMENT OF VARIABLE " &
                       "OF A FORMAL ACCESS TYPE FROM ANOTHER " &
                       "VARIABLE OF A FORMAL ACCESS TYPE");
          END IF;

          A := NEW INTEGER'(10);

          IF A.ALL /= 10 THEN
               FAILED ("IMPROPER VALUE FOR VARIABLE OF FORMAL ACCESS " &
                       "TYPE");
          END IF;

          IF A NOT IN ACC THEN
               FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST");
          END IF;

          B := ACC'(A);

          IF B.ALL /= 10 THEN
               FAILED ("IMPROPER VALUE FROM QUALIFICATION");
          END IF;

          DERA := NEW INTEGER'(10);
          A := ACC(DERA);

          IF A.ALL /= IDENT_INT(10) THEN
               FAILED ("IMPROPER VALUE FROM EXPLICIT CONVERSION");
          END IF;

          IF A.ALL > IDENT_INT(10) THEN
               FAILED ("IMPROPER VALUE USED IN LESS THAN");
          END IF;

          IF A.ALL < IDENT_INT(10) THEN
               FAILED ("IMPROPER VALUE USED IN GREATER THAN");
          END IF;

          IF A.ALL >= IDENT_INT(11) THEN
               FAILED ("IMPROPER VALUE USED IN LESS THAN OR EQUAL");
          END IF;

          IF A.ALL <= IDENT_INT(9) THEN
               FAILED ("IMPROPER VALUE USED IN GREATER THAN OR EQUAL");
          END IF;

          IF NOT (A.ALL + A.ALL = IDENT_INT(20)) THEN
               FAILED ("IMPROPER VALUE FROM ADDITION");
          END IF;

          IF NOT (A.ALL - IDENT_INT(2) = IDENT_INT(8)) THEN
               FAILED ("IMPROPER VALUE FROM SUBTRACTION");
          END IF;

          IF NOT (A.ALL * IDENT_INT(3) = IDENT_INT(30)) THEN
               FAILED ("IMPROPER VALUE FROM MULTIPLICATION");
          END IF;

          IF NOT (A.ALL / IDENT_INT(3) = IDENT_INT(3)) THEN
               FAILED ("IMPROPER VALUE FROM DIVISION");
          END IF;

          IF NOT (A.ALL MOD IDENT_INT(3) = IDENT_INT(1)) THEN
               FAILED ("IMPROPER VALUE FROM MODULO");
          END IF;

          IF NOT (A.ALL REM IDENT_INT(7) = IDENT_INT(3)) THEN
               FAILED ("IMPROPER VALUE FROM REMAINDER");
          END IF;

          IF NOT (A.ALL ** IDENT_INT(2) = IDENT_INT(100)) THEN
               FAILED ("IMPROPER VALUE FROM EXPONENTIATION");
          END IF;

          IF NOT (+A.ALL = IDENT_INT(10)) THEN
               FAILED ("IMPROPER VALUE FROM IDENTITY");
          END IF;

          IF NOT (-A.ALL = IDENT_INT(-10)) THEN
               FAILED ("IMPROPER VALUE FROM NEGATION");
          END IF;

          A := NULL;

          IF A /= NULL THEN
               FAILED ("IMPROPER VALUE FROM ACCESS SET TO NULL");
          END IF;

          IF A'ADDRESS NOT IN ADDRESS THEN
               FAILED ("IMPROPER RESULT FROM A'ADDRESS TEST");
          END IF;


          DECLARE
               ACC_SIZE : INTEGER := ACC'SIZE;
          BEGIN
               IF ACC_SIZE NOT IN INTEGER THEN
                    FAILED ("IMPROPER RESULT FROM ACC'SIZE");
               END IF;
          END;

          R := NEW REC'(COMP => 5);

          IF NOT EQUAL(R.COMP,5) THEN
               FAILED ("IMPROPER VALUE FOR RECORD COMPONENT");
          END IF;

          DR := NEW DISCREC'(DISC => 1, COMPD => 5);

          IF NOT EQUAL(DR.DISC,1) OR NOT EQUAL(DR.COMPD,5) THEN
               FAILED ("IMPROPER VALUES FOR DISCRIMINATED RECORD " &
                       "COMPONENTS");
          END IF;

          C := NEW ARR'(1 => (1,2), 2 => (3,4));

          IF C(1,1) /= 1 OR C(1,2) /= 2 OR C(2,1) /= 3 OR C(2,2) /= 4
               THEN FAILED ("IMPROPER ARRAY COMPONENT VALUES");
          END IF;

          D := NEW ONEDIM'(1,2,3,4,5,6,7,8,9,10);
          E := NEW ONEDIM'(10,9,8,7,6,5,4,3,2,1);

          D(1..5) := E(1..5);

          IF D(1) /= 10 OR D(2) /= 9 OR D(3) /= 8
               OR D(4) /= 7 OR D(5) /= 6 THEN
               FAILED ("IMPROPER RESULTS FROM SLICE ASSIGNMENT");
          END IF;

          IF C'FIRST /= 1 OR C'FIRST(2) /= 1 THEN
               FAILED ("IMPROPER LOWER BOUNDS FOR CONSTRAINED ARRAY");
          END IF;

          IF C'LAST /= 2 OR C'LAST(2) /= 2 THEN
               FAILED ("IMPROPER UPPER BOUNDS FOR CONSTRAINED ARRAY");
          END IF;

          IF 1 NOT IN C'RANGE THEN
               FAILED ("IMPROPER RANGE FOR CONSTRAINED ARRAY - 1");
          END IF;

          IF 1 NOT IN C'RANGE(2) THEN
               FAILED ("IMPROPER RANGE FOR CONSTRAINED ARRAY - 2");
          END IF;

          IF C'LENGTH /= 2 THEN
               FAILED ("IMPROPER NUMBER OF VALUES FOR CONSTRAINED " &
                       "ARRAY - 1");
          END IF;

          IF C'LENGTH(2) /= 2 THEN
               FAILED ("IMPROPER NUMBER OF VALUES FOR CONSTRAINED " &
                       "ARRAY - 2");
          END IF;

          F := NEW T;

          F.HERE(INT);

          IF NOT EQUAL(INT,IDENT_INT(10)) THEN
               FAILED ("IMPROPER RESULTS FROM ENTRY SELECTION");
          END IF;

          G := NEW T1;

          G.HERE1(TWO)(INT);

          IF NOT EQUAL(INT,IDENT_INT(20)) THEN
               FAILED ("IMPROPER RESULTS FROM FAMILY ENTRY SELECTION");
          END IF;

          RESULT;
     END P;

     PACKAGE PACK IS NEW P(INTEGER,ACCINTEGER,AI,AREC,ADISCREC,
                           AA,AONEDIM,ATASK,ATASK1,ANOTHERTASK);

BEGIN
     NULL;
END CC1225A;
