-- C97302A.ADA

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
-- CHECK THAT WHENEVER AN INDEX IS PRESENT IN A TIMED_ENTRY_CALL, IT
-- IS EVALUATED BEFORE ANY PARAMETER ASSOCIATIONS ARE EVALUATED, AND 
-- PARAMETER ASSOCIATIONS ARE EVALUATED BEFORE THE DELAY EXPRESSION.
-- THEN A RENDEZVOUS IS ATTEMPTED.

-- RJW 3/31/86

with Impdef;
WITH REPORT; USE REPORT;
WITH CALENDAR; USE CALENDAR;
PROCEDURE C97302A IS
     
     INDEX_COMPUTED  :  BOOLEAN  :=  FALSE;
     PARAM_COMPUTED  :  BOOLEAN  :=  FALSE;     
     DELAY_COMPUTED  :  BOOLEAN  :=  FALSE;
BEGIN

     TEST ("C97302A", "CHECK THAT WHENEVER AN INDEX IS PRESENT IN " &
                      "A TIMED_ENTRY_CALL, IT IS EVALUATED BEFORE " &
                      "ANY PARAMETER ASSOCIATIONS ARE EVALUATED, " &
                      "AND PARAMETER ASSOCIATIONS ARE EVALUATED " & 
                      "BEFORE THE DELAY EXPRESSION" );
     DECLARE

          WAIT_TIME : DURATION := 3.0 * Impdef.One_Second;
          
          TYPE SHORT IS RANGE 10 .. 20;

          TASK  T  IS
               ENTRY  DO_IT_NOW_OR_WAIT
                                      ( SHORT )
                                      ( DID_YOU_DO_IT : IN BOOLEAN );
               ENTRY  KEEP_ALIVE;
          END  T;

          TASK BODY  T  IS
          BEGIN
               ACCEPT  KEEP_ALIVE;
          END  T;

          FUNCTION  F1 (X : SHORT) RETURN SHORT IS
          BEGIN
               INDEX_COMPUTED  :=  TRUE;
               RETURN (15);
          END  F1;

          FUNCTION  F2 RETURN BOOLEAN  IS
          BEGIN
               IF INDEX_COMPUTED THEN
                    NULL;
               ELSE
                    FAILED ( "INDEX NOT EVALUATED FIRST" );
               END IF;
               PARAM_COMPUTED  :=  TRUE;
               RETURN (FALSE);
          END  F2;

          FUNCTION F3 RETURN DURATION IS
          BEGIN
               IF PARAM_COMPUTED THEN
                    NULL;
               ELSE
                    FAILED ( "PARAMETERS NOT EVALUATED BEFORE DELAY " &
                             "EXPRESSION" );
               END IF;
               DELAY_COMPUTED := TRUE;
               RETURN (WAIT_TIME);
          END;
     BEGIN

          SELECT
               T.DO_IT_NOW_OR_WAIT
                                      ( F1 (15) )
                                      ( NOT F2 );
               FAILED ("RENDEZVOUS OCCURRED");
          OR
               DELAY F3;          
          END SELECT;

          T.KEEP_ALIVE;

     END;   -- END OF BLOCK CONTAINING THE ENTRY CALLS.

     IF  DELAY_COMPUTED  THEN
          NULL;
     ELSE
          FAILED( "DELAY EXPRESSION NOT EVALUATED" );
     END IF;

     RESULT;

END  C97302A; 
