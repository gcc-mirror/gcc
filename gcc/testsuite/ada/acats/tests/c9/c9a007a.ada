-- C9A007A.ADA

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
-- CHECK THAT A TASK MAY ABORT A TASK IT DEPENDS ON.


-- RM 5/26/82
-- RM 7/02/82
-- SPS 11/21/82
-- JBG 2/27/84
-- JBG 3/8/84
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.
-- EDS 08/04/98 ENSURE THAT ABORTED TASKS HAVE TIME TO EFFECT THEIR ABORTIONS.

WITH IMPDEF;
WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;
PROCEDURE  C9A007A  IS

      TASK_NOT_ABORTED : BOOLEAN := FALSE; 
      TEST_VALID       : BOOLEAN := TRUE ; 

BEGIN


     -------------------------------------------------------------------


     TEST ( "C9A007A" , "CHECK THAT A TASK MAY ABORT A TASK" &
                        " IT DEPENDS ON"                     );

  
     DECLARE


          TASK  REGISTER  IS


               ENTRY  BIRTHS_AND_DEATHS; 

               ENTRY  SYNC1; 
               ENTRY  SYNC2; 


          END  REGISTER; 


          TASK BODY  REGISTER  IS


               TASK TYPE  SECONDARY  IS


                    ENTRY  WAIT_INDEFINITELY; 

               END  SECONDARY; 


               TASK TYPE  T_TYPE1  IS


                    ENTRY  E; 

               END  T_TYPE1; 


               TASK TYPE  T_TYPE2  IS


                    ENTRY  E; 

               END  T_TYPE2; 


               T_OBJECT1 : T_TYPE1; 
               T_OBJECT2 : T_TYPE2; 


               TASK BODY  SECONDARY  IS
               BEGIN
                    SYNC1; 
                    ABORT  T_OBJECT1; 
                    DELAY 0.0; 
                    TASK_NOT_ABORTED  :=  TRUE; 
               END  SECONDARY; 


               TASK BODY  T_TYPE1  IS

                    TYPE  ACCESS_TO_TASK  IS  ACCESS SECONDARY; 
  
               BEGIN


                    DECLARE
                         DEPENDENT_BY_ACCESS   :  ACCESS_TO_TASK  :=
                                                  NEW  SECONDARY ; 
                    BEGIN
                         NULL;
                    END; 


                    BIRTHS_AND_DEATHS; 
                                     -- DURING THIS SUSPENSION
                                     --     MOST OF THE TASKS
                                     --     ARE ABORTED   (FIRST
                                     --     TASK #1    -- T_OBJECT1 --
                                     --     THEN  #2 ).


                    TASK_NOT_ABORTED := TRUE; 


               END  T_TYPE1; 


               TASK BODY  T_TYPE2  IS

                    TASK  INNER_TASK  IS


                         ENTRY  WAIT_INDEFINITELY; 

                    END  INNER_TASK; 

                    TASK BODY  INNER_TASK  IS
                    BEGIN
                         SYNC2; 
                         ABORT  T_OBJECT2; 
                         DELAY 0.0; 
                         TASK_NOT_ABORTED  :=  TRUE; 
                    END  INNER_TASK; 

               BEGIN


                    BIRTHS_AND_DEATHS; 
                                     -- DURING THIS SUSPENSION
                                     --     MOST OF THE TASKS
                                     --     ARE ABORTED   (FIRST
                                     --     TASK #1     -- T_OBJECT1 --
                                     --     THEN  #2 ).


                    TASK_NOT_ABORTED := TRUE; 


               END  T_TYPE2; 


          BEGIN

               DECLARE
                    OLD_COUNT : INTEGER := 0; 
               BEGIN


                    FOR  I  IN  1..5  LOOP
                         EXIT WHEN  BIRTHS_AND_DEATHS'COUNT = 2; 
                         DELAY 10.0 * Impdef.One_Second;
                    END LOOP;

                    OLD_COUNT := BIRTHS_AND_DEATHS'COUNT; 

                    IF  OLD_COUNT = 2  THEN

                         ACCEPT  SYNC1;   -- ALLOWING  ABORT#1

                         DELAY IMPDEF.CLEAR_READY_QUEUE;

                         -- CHECK THAT  #1  WAS ABORTED  -  3 WAYS:

                         BEGIN
                              T_OBJECT1.E; 
                              FAILED( "T_OBJECT1.E  DID NOT RAISE" &
                                                   "  TASKING_ERROR" );
                         EXCEPTION

                              WHEN TASKING_ERROR  =>
                                   NULL;

                              WHEN OTHERS  =>
                                   FAILED("OTHER EXCEPTION RAISED - 1");

                         END; 

                         IF T_OBJECT1'CALLABLE  THEN
                              FAILED( "T_OBJECT1'CALLABLE = TRUE" );
                         END IF;

                         IF  OLD_COUNT - BIRTHS_AND_DEATHS'COUNT /= 1
                         THEN
                              FAILED( "TASK#1 NOT REMOVED FROM QUEUE" );
                         END IF;


                         OLD_COUNT := BIRTHS_AND_DEATHS'COUNT; 


                         ACCEPT  SYNC2;   -- ALLOWING  ABORT#2
                         
                         DELAY IMPDEF.CLEAR_READY_QUEUE;

                         -- CHECK THAT  #2  WAS ABORTED  -  3 WAYS:

                         BEGIN
                              T_OBJECT2.E; 
                              FAILED( "T_OBJECT2.E  DID NOT RAISE" &
                                                   "  TASKING_ERROR" );
                         EXCEPTION

                              WHEN TASKING_ERROR  =>
                                   NULL;

                              WHEN OTHERS  =>
                                   FAILED("OTHER EXCEPTION RAISED - 2");

                         END; 

                         IF T_OBJECT2'CALLABLE  THEN
                              FAILED( "T_OBJECT2'CALLABLE = TRUE" );
                         END IF;

                         IF  OLD_COUNT - BIRTHS_AND_DEATHS'COUNT /= 1
                         THEN
                              FAILED( "TASK#2 NOT REMOVED FROM QUEUE" );
                         END IF;


                         IF  BIRTHS_AND_DEATHS'COUNT /= 0  THEN
                              FAILED( "SOME TASKS STILL QUEUED" );
                         END IF;
  

                    ELSE

                         COMMENT( "LINEUP NOT COMPLETE (AFTER 50 S.)" );
                         TEST_VALID  :=  FALSE; 

                    END IF;


               END; 


               WHILE  BIRTHS_AND_DEATHS'COUNT > 0  LOOP
                    ACCEPT  BIRTHS_AND_DEATHS; 
               END LOOP;


          END  REGISTER; 


     BEGIN

          NULL;

     END; 


     -------------------------------------------------------------------


     IF  TEST_VALID  AND  TASK_NOT_ABORTED  THEN
          FAILED( "SOME TASKS NOT ABORTED" );
     END IF;


     RESULT;


END  C9A007A; 
