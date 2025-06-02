-- CC3605A.ADA

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
--     CHECK THAT SOME DIFFERENCES BETWEEN THE FORMAL AND THE
--     ACTUAL SUBPROGRAMS DO NOT INVALIDATE A MATCH.
--          1)  CHECK DIFFERENT PARAMETER NAMES.
--          2)  CHECK DIFFERENT PARAMETER CONSTRAINTS.
--          3)  CHECK ONE PARAMETER CONSTRAINED AND THE OTHER
--               UNCONSTRAINED (WITH ARRAY, RECORD, ACCESS, AND
--               PRIVATE TYPES).
--          4)  CHECK PRESENCE OR ABSENCE OF AN EXPLICIT "IN" MODE
--               INDICATOR.
--          5)  DIFFERENT TYPE MARKS USED TO SPECIFY THE TYPE OF
--               PARAMETERS.

-- HISTORY:
--     LDC 10/04/88  CREATED ORIGINAL TEST.

PACKAGE CC3605A_PACK IS

     SUBTYPE INT IS INTEGER RANGE -100 .. 100;

     TYPE PRI_TYPE (SIZE : INT) IS PRIVATE;

     SUBTYPE PRI_CONST IS PRI_TYPE (2);

PRIVATE

     TYPE ARR_TYPE IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;

     TYPE PRI_TYPE (SIZE : INT) IS
          RECORD
               SUB_A : ARR_TYPE (1 .. SIZE);
          END RECORD;

END CC3605A_PACK;


WITH REPORT;
USE  REPORT;
WITH CC3605A_PACK;
USE  CC3605A_PACK;

PROCEDURE CC3605A IS

     SUBTYPE ZERO_TO_TEN IS INTEGER
          RANGE IDENT_INT (0) .. IDENT_INT (10);

     SUBTYPE ONE_TO_FIVE IS INTEGER
          RANGE IDENT_INT (1) .. IDENT_INT (5);

     SUBPRG_ACT : BOOLEAN := FALSE;
BEGIN
     TEST
          ("CC3605A", "CHECK THAT SOME DIFFERENCES BETWEEN THE " &
                      "FORMAL AND THE ACTUAL PARAMETERS DO NOT " &
                      "INVALIDATE A MATCH");

----------------------------------------------------------------------
-- DIFFERENT PARAMETER NAMES
----------------------------------------------------------------------

     DECLARE

          PROCEDURE ACT_PROC (DIFF_NAME_PARM : ONE_TO_FIVE) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : ONE_TO_FIVE);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (ONE_TO_FIVE'FIRST);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                    ("DIFFERENT PARAMETER NAMES MADE MATCH INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- DIFFERENT PARAMETER CONSTRAINTS
----------------------------------------------------------------------

     DECLARE

          PROCEDURE ACT_PROC (PARM : ONE_TO_FIVE) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : ZERO_TO_TEN);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (ONE_TO_FIVE'FIRST);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                    ("DIFFERENT PARAMETER CONSTRAINTS MADE MATCH " &
                     "INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (ARRAY)
----------------------------------------------------------------------

     DECLARE

          TYPE ARR_TYPE IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;

          SUBTYPE ARR_CONST IS ARR_TYPE (ONE_TO_FIVE'FIRST ..
               ONE_TO_FIVE'LAST);

          PASSED_PARM : ARR_CONST := (OTHERS => TRUE);

          PROCEDURE ACT_PROC (PARM : ARR_CONST) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : ARR_TYPE);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (PASSED_PARM);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                    ("ONE ARRAY PARAMETER CONSTRAINED MADE MATCH " &
                     "INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (RECORDS)
----------------------------------------------------------------------

     DECLARE

          TYPE REC_TYPE (BOL : BOOLEAN) IS
               RECORD
                    SUB_A : INTEGER;
                    CASE BOL IS
                         WHEN TRUE =>
                              DSCR_A : INTEGER;

                         WHEN FALSE =>
                              DSCR_B : BOOLEAN;

                    END CASE;
               END RECORD;

          SUBTYPE REC_CONST IS REC_TYPE (TRUE);

          PASSED_PARM : REC_CONST := (TRUE, 1, 2);

          PROCEDURE ACT_PROC (PARM : REC_CONST) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : REC_TYPE);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (PASSED_PARM);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                    ("ONE RECORD PARAMETER CONSTRAINED MADE MATCH " &
                     "INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (ACCESS)
----------------------------------------------------------------------

     DECLARE

          TYPE ARR_TYPE IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;

          SUBTYPE ARR_CONST     IS ARR_TYPE (ONE_TO_FIVE'FIRST ..
               ONE_TO_FIVE'LAST);

          TYPE ARR_ACC_TYPE IS ACCESS ARR_TYPE;

          SUBTYPE ARR_ACC_CONST IS ARR_ACC_TYPE (1 .. 3);

          PASSED_PARM : ARR_ACC_TYPE := NULL;

          PROCEDURE ACT_PROC (PARM : ARR_ACC_CONST) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : ARR_ACC_TYPE);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (PASSED_PARM);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                    ("ONE ACCESS PARAMETER CONSTRAINED MADE MATCH " &
                     "INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- ONE PARAMETER CONSTRAINED (PRIVATE)
----------------------------------------------------------------------

     DECLARE
          PASSED_PARM : PRI_CONST;

          PROCEDURE ACT_PROC (PARM : PRI_CONST) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : PRI_TYPE);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (PASSED_PARM);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                    ("ONE PRIVATE PARAMETER CONSTRAINED MADE MATCH " &
                     "INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- PRESENCE (OR ABSENCE) OF AN EXPLICIT "IN" MODE
----------------------------------------------------------------------

     DECLARE

          PROCEDURE ACT_PROC (PARM : INTEGER) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM : IN INTEGER);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (1);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED
                     ("PRESENCE OF AN EXPLICIT 'IN' MODE MADE MATCH " &
                     "INVALID");
          END IF;
     END;

----------------------------------------------------------------------
-- DIFFERENT TYPE MARKS
----------------------------------------------------------------------

     DECLARE

          SUBTYPE MARK_1_TYPE IS INTEGER;

          SUBTYPE MARK_2_TYPE IS INTEGER;

          PROCEDURE ACT_PROC (PARM1 : IN MARK_1_TYPE) IS
          BEGIN
               SUBPRG_ACT := TRUE;
          END ACT_PROC;

          GENERIC

               WITH PROCEDURE PASSED_PROC (PARM2 : MARK_2_TYPE);

          PROCEDURE GEN_PROC;

          PROCEDURE GEN_PROC IS
          BEGIN
               PASSED_PROC (1);
          END GEN_PROC;

          PROCEDURE INST_PROC IS NEW GEN_PROC (ACT_PROC);
     BEGIN
          SUBPRG_ACT := FALSE;
          INST_PROC;
          IF NOT SUBPRG_ACT THEN
               FAILED ("DIFFERENT TYPE MARKS MADE MATCH INVALID");
          END IF;
     END;
     RESULT;
END CC3605A;
