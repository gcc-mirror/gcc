-- C64201C.ADA

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
-- CHECK THAT INITIALIZATION OF IN PARAMETERS OF A COMPOSITE
--   TYPE HAVING AT LEAST ONE COMPONENT (INCLUDING COMPONENTS
--   OF COMPONENTS) OF A TASK TYPE IS PERMITTED.
--  (SEE ALSO 7.4.4/T2 FOR TESTS OF LIMITED PRIVATE TYPES.)

-- CVP 5/14/81
-- ABW 7/1/82
-- BHS 7/9/84

WITH REPORT;
USE REPORT;
PROCEDURE C64201C IS


     GLOBAL : INTEGER := 10;


     TASK TYPE T IS
          ENTRY E (X : IN OUT INTEGER);
     END;

     TYPE REC_T IS
          RECORD
               TT : T;
               BB : BOOLEAN := TRUE;
          END RECORD;

     TYPE REC_REC_T IS
          RECORD
               RR : REC_T;
          END RECORD;

     TYPE ARR_T IS ARRAY (1 .. 2) OF T;

     TYPE ARR_REC_T IS ARRAY (1 .. 2) OF REC_T;

     RT1, RT2   : REC_T;
     RRT1, RRT2 : REC_REC_T;
     AT1, AT2   : ARR_T;
     ART1, ART2 : ARR_REC_T;

     
     TASK BODY T IS
     BEGIN
          ACCEPT E (X : IN OUT INTEGER) DO
               X := X - 1;
          END E;
          ACCEPT E (X : IN OUT INTEGER) DO
               X := X + 1;
          END E;
     END T;


     PROCEDURE PROC1A (P1X : REC_T := RT1) IS
     BEGIN
          IF P1X.BB THEN                 -- EXPECT RT2 PASSED.
               FAILED( "RECORD OF TASK NOT PASSED, DEFAULT EMPLOYED" );
          END IF;
     END PROC1A;

     PROCEDURE PROC1B (P1X : REC_T := RT1) IS
     BEGIN
          IF NOT P1X.BB THEN             -- EXPECT DEFAULT USED.
               FAILED( "DEFAULT RECORD OF TASK NOT EMPLOYED" );
          END IF;
     END PROC1B;


     PROCEDURE PROC2A (P2X : REC_REC_T := RRT1) IS
     BEGIN
          IF P2X.RR.BB THEN             -- EXPECT RRT2 PASSED.
               FAILED( "RECORD OF RECORD OF TASK NOT PASSED, " &
                       "DEFAULT EMPLOYED" );
          END IF;
     END PROC2A;

     PROCEDURE PROC2B (P2X : REC_REC_T := RRT1) IS
     BEGIN
          IF NOT P2X.RR.BB THEN         -- EXPECT DEFAULT USED.
               FAILED( "DEFAULT RECORD OF RECORD OF TASK " &
                       "NOT EMPLOYED" );
          END IF;
     END PROC2B;


     PROCEDURE PROC3 (P3X : ARR_T := AT1) IS
     BEGIN
          P3X(1).E (X => GLOBAL);        -- CALL TO AT2(1).E,
                                         -- GLOBAL => GLOBAL - 1.
     END PROC3;

     PROCEDURE PROC4 (P4X : ARR_T := AT1) IS
     BEGIN
          P4X(1).E (X => GLOBAL);     -- CALL TO DEFAULT AT1(1).E,
                                      -- GLOBAL => GLOBAL - 1.
          IF GLOBAL /= IDENT_INT(8) THEN
               FAILED( "ARRAY OF TASKS NOT PASSED " &
                       "CORRECTLY IN PROC3" );
          END IF;
     END PROC4;

     PROCEDURE PROC5 (P5X : ARR_REC_T := ART1) IS
     BEGIN
          P5X(1).TT.E (X => GLOBAL);      -- CALL TO ART2(1).TT.E,
                                          -- GLOBAL => GLOBAL - 1.
     END PROC5;

     PROCEDURE PROC6 (P6X : ARR_REC_T := ART1) IS
     BEGIN
          P6X(1).TT.E (X => GLOBAL);      -- CALL DEFAULT ART1(1).TT.E,
                                          -- GLOBAL => GLOBAL - 1.
          IF GLOBAL /= IDENT_INT(8) THEN
               FAILED( "ARRAY OF RECORDS OF TASKS NOT " &
                       "PASSED IN PROC5" );
          END IF;
     END PROC6;

     PROCEDURE TERM (TSK : T; NUM : CHARACTER) IS
     BEGIN
          IF NOT TSK'TERMINATED THEN
               ABORT TSK;
               COMMENT ("ABORTING TASK " & NUM);
          END IF;
     END TERM;


BEGIN

     TEST( "C64201C" , "CHECK THAT INITIALIZATION OF IN " &
                       "PARAMETERS OF A COMPOSITE TYPE " &
                       "IS PERMITTED" );

     RT2.BB := FALSE;
     RRT2.RR.BB := FALSE;

     PROC1A(RT2);                               -- NO ENTRY CALL
     PROC1B;                                    -- NO ENTRY CALL
     PROC2A(RRT2);                              -- NO ENTRY CALL
     PROC2B;                                    -- NO ENTRY CALL

     PROC3(AT2);                                -- CALL AT2(1).E
     IF GLOBAL /= 9 THEN
          FAILED ("INCORRECT GLOBAL VALUE AFTER PROC3");
     ELSE
          PROC4;                                -- CALL AT1(1).E
     END IF;

     GLOBAL := 10;
     PROC5(ART2);                              -- CALL ART2(1).TT.E
     IF GLOBAL /= 9 THEN
          FAILED ("INCORRECT GLOBAL VALUE AFTER PROC5");
     ELSE
          PROC6;                               -- CALL ART1(1).TT.E
     END IF;

-- MAKE SURE ALL TASKS TERMINATED
     TERM (RT1.TT, '1');
     TERM (RT2.TT, '2');
     TERM (RRT1.RR.TT, '3');
     TERM (RRT2.RR.TT, '4');
     TERM (AT1(1), '5');
     TERM (AT2(1), '6');
     TERM (AT1(2), '7');
     TERM (AT2(2), '8');
     TERM (ART1(1).TT, '9');
     TERM (ART2(1).TT, 'A');
     TERM (ART1(2).TT, 'B');
     TERM (ART2(2).TT, 'C');

     RESULT;

END C64201C;
