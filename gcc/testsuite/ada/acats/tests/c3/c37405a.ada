-- C37405A.ADA

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
-- CHECK THAT WHEN ASSIGNING TO A CONSTRAINED OR UNCONSTRAINED
-- OBJECT OR FORMAL PARAMETER OF A TYPE DECLARED WITH DEFAULT
-- DISCRIMINANTS, THE ASSIGNMENT DOES NOT CHANGE THE 'CONSTRAINED
-- ATTRIBUTE VALUE ASSOCIATED WITH THE OBJECT OR PARAMETER.
 
-- ASL 7/21/81
-- TBN 1/20/86     RENAMED FROM C37209A.ADA AND REVISED THE ASSIGNMENTS 
--                 OF CONSTRAINED AND UNCONSTRAINED OBJECTS TO ARRAY AND
--                 RECORD COMPONENTS.

WITH REPORT; USE REPORT;
PROCEDURE C37405A IS
 
     TYPE REC(DISC : INTEGER := 25) IS
          RECORD
               COMP : INTEGER;
          END RECORD;
 
     SUBTYPE CONSTR IS REC(10);
     SUBTYPE UNCONSTR IS REC;

     TYPE REC_C IS 
          RECORD
               COMP: CONSTR;
          END RECORD;

     TYPE REC_U IS
          RECORD
               COMP: UNCONSTR;
          END RECORD;
 
     C1,C2 : CONSTR;
     U1,U2 : UNCONSTR;
-- C2 AND U2 ARE NOT PASSED TO EITHER PROC1 OR PROC2.

     ARR_C : ARRAY (1..5) OF CONSTR;
     ARR_U : ARRAY (1..5) OF UNCONSTR;

     REC_COMP_C : REC_C;
     REC_COMP_U : REC_U;
 
     PROCEDURE PROC11(PARM : IN OUT UNCONSTR; B : IN BOOLEAN) IS
     BEGIN
          PARM := C2;
          IF IDENT_BOOL(B) /= PARM'CONSTRAINED THEN
               FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY " &
                       "ASSIGNMENT - 1");
          END IF;
     END PROC11;

     PROCEDURE PROC12(PARM : IN OUT UNCONSTR; B : IN BOOLEAN) IS
     BEGIN
          PARM := U2;
          IF B /= PARM'CONSTRAINED THEN
               FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY " &
                       "ASSIGNMENT - 2");
          END IF;
     END PROC12;

     PROCEDURE PROC1(PARM : IN OUT UNCONSTR; B : IN BOOLEAN) IS
     BEGIN
          IF B /= PARM'CONSTRAINED THEN
               FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY " &
                       "PASSING PARAMETER");
          END IF;
 
          PROC11(PARM, B);
 
          PROC12(PARM, B);

     END PROC1;
 
     PROCEDURE PROC2(PARM : IN OUT CONSTR) IS
     BEGIN
          COMMENT ("CALLING PROC1 FROM PROC2");   -- IN CASE TEST FAILS.
          PROC1(PARM,TRUE);
          PARM := U2;
          IF NOT PARM'CONSTRAINED THEN
               FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY " &
                       "ASSIGNMENT - 3");
          END IF;
     END PROC2;
BEGIN
     TEST("C37405A", "'CONSTRAINED ATTRIBUTE OF OBJECTS, FORMAL " &
                     "PARAMETERS CANNOT BE CHANGED BY ASSIGNMENT");
 
     C2 := (DISC => IDENT_INT(10), COMP => 3);
     U2 := (DISC => IDENT_INT(10), COMP => 4);

     ARR_C := (1..5 => U2);
     ARR_U := (1..5 => C2);

     REC_COMP_C := (COMP => U2);
     REC_COMP_U := (COMP => C2);
 
     C1 := U2;
     U1 := C2;
 
     IF U1'CONSTRAINED OR NOT C1'CONSTRAINED THEN
          FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY ASSIGNMENT - 4");
     END IF;

     IF ARR_U(3)'CONSTRAINED OR NOT ARR_C(4)'CONSTRAINED THEN
          FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY ASSIGNMENT - 5");
     END IF;

     IF REC_COMP_U.COMP'CONSTRAINED
         OR NOT REC_COMP_C.COMP'CONSTRAINED THEN
          FAILED ("'CONSTRAINED ATTRIBUTE CHANGED BY ASSIGNMENT - 6");
     END IF;
 
     COMMENT("CALLING PROC1 DIRECTLY");
     PROC1(C1,TRUE);
     PROC2(C1);

     COMMENT("CALLING PROC1 DIRECTLY");
     PROC1(U1,FALSE);
     PROC2(U1);

     COMMENT("CALLING PROC1 DIRECTLY");
     PROC1(ARR_C(4), TRUE);
     PROC2(ARR_C(5));

     COMMENT("CALLING PROC1 DIRECTLY");
     PROC1(ARR_U(2), FALSE);
     PROC2(ARR_U(3));

     COMMENT("CALLING PROC1 DIRECTLY");
     PROC1(REC_COMP_C.COMP, TRUE);
     PROC2(REC_COMP_C.COMP);

     COMMENT("CALLING PROC1 DIRECTLY");
     PROC1(REC_COMP_U.COMP, FALSE);
     PROC2(REC_COMP_U.COMP);
 
     RESULT;
END C37405A;
