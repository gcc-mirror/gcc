-- C93005B.ADA

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
-- CHECK THAT WHEN AN EXCEPTION IS RAISED IN A DECLARATIVE PART, A TASK
-- DECLARED IN THE SAME DECLARATIVE PART BECOMES TERMINATED.

-- CHECK THAT A TASK WAITING ON ENTRIES OF SUCH A
-- TERMINATED-BEFORE-ACTIVATION TASK RECEIVES TASKING_ERROR.

-- THIS TEST CHECKS THE CASE IN WHICH SEVERAL TASKS ARE WAITING FOR
-- ACTIVATION WHEN THE EXCEPTION OCCURS.

-- R. WILLIAMS 8/7/86
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C93005B IS


BEGIN
     TEST ( "C93005B", "CHECK THAT WHEN AN EXCEPTION IS RAISED IN A " &
                       "DECLARATIVE PART, A TASK DECLARED IN THE " &
                       "SAME DECLARATIVE PART BECOMES TERMINATED. " &
                       "IN THIS CASE, SEVERAL TASKS ARE WAITING FOR " &
                       "ACTIVATION WHEN THE EXCEPTION OCCURS" );
 
     BEGIN
 
          DECLARE
               TASK TYPE TA IS      -- CHECKS THAT TX TERMINATES.
               END TA;
  
               TYPE ATA IS ACCESS TA;
 
               TASK TYPE TB IS      -- CHECKS THAT TY TERMINATES.
               END TB;
                 
               TYPE TBREC IS
                    RECORD
                         TTB: TB;
                    END RECORD;
  
               TASK TX IS          -- WILL NEVER BE ACTIVATED.
                    ENTRY E;
               END TX;

               TASK BODY TA IS
               BEGIN
                    DECLARE  -- THIS BLOCK TO CHECK THAT TAB 
                             -- TERMINATES.
                         TASK TAB IS
                         END TAB;

                         TASK BODY TAB IS
                         BEGIN
                              TX.E;
                              FAILED ( "RENDEZVOUS COMPLETED " &
                                       "WITHOUT ERROR - TAB" );
                         EXCEPTION
                              WHEN TASKING_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "ABNORMAL EXCEPTION " &
                                            "- TAB" );
                         END TAB;
                    BEGIN
                         NULL;
                    END;
  
                    TX.E;    --TX IS NOW TERMINATED.
 
                    FAILED ( "RENDEZVOUS COMPLETED WITHOUT ERROR " &
                             "- TA" );
  
               EXCEPTION
                    WHEN TASKING_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "ABNORMAL EXCEPTION - TA" );
               END TA;
 
               PACKAGE RAISE_IT IS 
                    TASK TY IS             -- WILL NEVER BE ACTIVATED.
                         ENTRY E;
                    END TY;                                         
               END RAISE_IT;
 
               TASK BODY TB IS
               BEGIN
                    DECLARE  -- THIS BLOCK TO CHECK THAT TBB 
                             -- TERMINATES.
                         TASK TBB IS
                         END TBB;
 
                         TASK BODY TBB IS
                         BEGIN
                              RAISE_IT.TY.E;
                              FAILED ( "RENDEZVOUS COMPLETED " &
                                       "WITHOUT ERROR - TBB" );
                         EXCEPTION
                              WHEN TASKING_ERROR =>
                                   NULL;
                              WHEN OTHERS =>
                                   FAILED ( "ABNORMAL EXCEPTION " &
                                            "- TBB" );
                         END TBB;
                    BEGIN
                         NULL;
                    END;

                    RAISE_IT.TY.E;    -- TY IS NOW TERMINATED.

                    FAILED ( "RENDEZVOUS COMPLETED WITHOUT ERROR " &
                             "- TB" );

               EXCEPTION
                    WHEN TASKING_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "ABNORMAL EXCEPTION - TB" );
               END TB;

               PACKAGE START_TC IS END START_TC;

               TASK BODY TX IS
               BEGIN
                    FAILED ( "TX ACTIVATED" );
                    -- IN CASE OF FAILURE.
                    LOOP
                         SELECT
                              ACCEPT E;
                         OR
                              TERMINATE;
                         END SELECT;
                    END LOOP;
               END TX;

               PACKAGE START_TZ IS
                    TASK TZ IS             -- WILL NEVER BE ACTIVATED.
                         ENTRY E;
                    END TZ;
               END START_TZ;

               PACKAGE BODY START_TC IS
                    TBREC1 : TBREC;     -- CHECKS THAT TY TERMINATES.

                    TASK TC IS -- CHECKS THAT TZ TERMINATES.
                    END TC;
                
                    TASK BODY TC IS
                    BEGIN
                         DECLARE  -- THIS BLOCK TO CHECK THAT TCB 
                                  -- TERMINATES.
 
                              TASK TCB IS
                              END TCB;
          
                              TASK BODY TCB IS
                              BEGIN
                                   START_TZ.TZ.E;
                                   FAILED ( "RENDEZVOUS COMPLETED " &
                                            "WITHOUT " &
                                            "ERROR - TCB" );
                              EXCEPTION
                                   WHEN TASKING_ERROR =>
                                        NULL;
                                   WHEN OTHERS =>
                                        FAILED ( "ABNORMAL " &
                                                 "EXCEPTION - TCB" );
                              END TCB;
                         BEGIN
                              NULL;
                         END;

                         START_TZ.TZ.E;    -- TZ IS NOW TERMINATED.
      
                         FAILED ( "RENDEZVOUS COMPLETED WITHOUT " &
                                  "ERROR - TC" );
      
                    EXCEPTION
                         WHEN TASKING_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ( "ABNORMAL EXCEPTION - TC" );
                    END TC;
               END START_TC;     -- TBREC1 AND TC ACTIVATED HERE.
                     
               PACKAGE BODY RAISE_IT IS
                    NTA : ATA := NEW TA;  -- NTA.ALL ACTIVATED HERE.
 
                    TASK BODY TY IS
                    BEGIN
                         FAILED ( "TY ACTIVATED" );
                         -- IN CASE OF FAILURE.
                         LOOP
                              SELECT
                                   ACCEPT E;
                              OR
                                   TERMINATE;
                              END SELECT;
                         END LOOP;
                    END TY;
 
                    PACKAGE XCEPTION IS
                         I : POSITIVE := IDENT_INT (0); -- RAISE
                                                   -- CONSTRAINT_ERROR.
                    END XCEPTION;
                
                    USE XCEPTION;
 
               BEGIN   -- TY WOULD BE ACTIVATED HERE.

                    IF I /= IDENT_INT (2) OR I = IDENT_INT (1) + 1 THEN
                         FAILED ( "PACKAGE DIDN'T RAISE EXCEPTION" );
                    END IF;
               END RAISE_IT;
 
               PACKAGE BODY START_TZ IS
                    TASK BODY TZ IS
                    BEGIN
                         FAILED ( "TZ ACTIVATED" );
                         -- IN CASE OF FAILURE.
                         LOOP
                              SELECT
                                   ACCEPT E;
                              OR
                                   TERMINATE;
                              END SELECT;
                         END LOOP;
                    END TZ;
               END START_TZ;    -- TZ WOULD BE ACTIVATED HERE.
 
          BEGIN     -- TX WOULD BE ACTIVATED HERE.
                    -- CAN'T LEAVE BLOCK UNTIL TA, TB, AND TC ARE TERM.

               FAILED ( "EXCEPTION NOT RAISED" );
          END;
 
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN TASKING_ERROR =>
               FAILED ( "TASKING_ERROR IN MAIN PROGRAM" );
          WHEN OTHERS =>
               FAILED ( "ABNORMAL EXCEPTION IN MAIN" );
     END;
 
     RESULT;
 
END C93005B;
