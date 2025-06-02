-- C93004F.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED DURING THE ACTIVATION OF A
-- TASK, OTHER TASKS ARE UNAFFECTED.

-- THE ENCLOSING BLOCK RECEIVES TASKING_ERROR.

-- THIS TESTS CHECKS THE CASE IN WHICH THE TASKS ARE CREATED BY THE
-- ALLOCATION OF A RECORD OF TASKS OR AN ARRAY OF TASKS.

-- R. WILLIAMS 8/7/86

WITH REPORT; USE REPORT;

PROCEDURE C93004F IS

BEGIN
     TEST ( "C93004F", "CHECK THAT WHEN AN EXCEPTION IS RAISED " &
                       "DURING THE ACTIVATION OF A TASK, OTHER " &
                       "TASKS ARE UNAFFECTED. IN THIS TEST, THE " &
                       "TASKS ARE CREATED BY THE ALLOCATION OF A " &
                       "RECORD OR AN ARRAY OF TASKS" );

     DECLARE

          TASK TYPE T IS 
               ENTRY E;
          END T;

          TASK TYPE TT;

          TASK TYPE TX IS 
               ENTRY E;
          END TX;

          TYPE REC IS
               RECORD
                    TR : T;
               END RECORD;

          TYPE ARR IS ARRAY (IDENT_INT (1) .. IDENT_INT (1)) OF T;

          TYPE RECX IS
               RECORD 
                    TTX1 : TX;
                    TTT  : TT;
                    TTX2 : TX;
               END RECORD;

          TYPE ACCR IS ACCESS REC;
          AR : ACCR;

          TYPE ACCA IS ACCESS ARR;
          AA : ACCA;
               
          TYPE ACCX IS ACCESS RECX;
          AX : ACCX;

          TASK BODY T IS
          BEGIN
               ACCEPT E;
          END T;

          TASK BODY TT IS
          BEGIN
               AR.TR.E;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ( "TASK AR.TR NOT ACTIVE" );
          END TT;

          TASK BODY TX IS
               I : POSITIVE := IDENT_INT (0); -- RAISE 
                                              -- CONSTRAINT_ERROR.
          BEGIN
               IF I /= IDENT_INT (2) OR I = IDENT_INT (1) + 1 THEN
                    FAILED ( "TX ACTIVATED OK" );
               END IF;
          END TX;

     BEGIN
          AR := NEW REC;
          AA := NEW ARR;
          AX := NEW RECX;

          FAILED ( "TASKING_ERROR NOT RAISED IN MAIN" );
          
          AA.ALL (1).E;        -- CLEAN UP.

     EXCEPTION
          WHEN TASKING_ERROR =>

               BEGIN
                    AA.ALL (1).E;
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         FAILED ( "AA.ALL (1) NOT ACTIVATED" );
               END;

          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED IN MAIN" );
          WHEN OTHERS =>
               FAILED ( "ABNORMAL EXCEPTION IN MAIN" );
     END;

     RESULT;

END C93004F;
