-- C59002C.ADA

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
-- CHECK THAT JUMPS OUT OF SELECT STATEMENTS (OTHER THAN
--    FROM INSIDE  ACCEPT  BODIES IN SELECT_ALTERNATIVES)
--    ARE POSSIBLE AND ARE CORRECTLY PERFORMED.

-- THIS TEST CONTAINS SHARED VARIABLES.


-- RM 08/15/82
-- SPS 12/13/82
-- PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES FOR ADA 9X.

with Impdef;
WITH REPORT;
WITH SYSTEM;
USE SYSTEM;
PROCEDURE  C59002C  IS

     USE  REPORT ;

     FLOW_STRING : STRING(1..2) := "XX" ;
     INDEX       : INTEGER      :=  1 ;


BEGIN

     TEST( "C59002C" , "CHECK THAT ONE CAN JUMP OUT OF SELECT STATE" &
                       "MENTS" );

     -------------------------------------------------------------------

     DECLARE

          TASK  T  IS


               ENTRY  E1 ;
               ENTRY  E2 ;
          END  T ;

          TASK BODY T IS
          BEGIN

               WHILE  E2'COUNT <= 0  LOOP
                    DELAY 1.0 * Impdef.One_Second;
               END LOOP;

               SELECT 
                    ACCEPT  E1  DO
                         FAILED( " E1  ACCEPTED; NO ENTRY CALL (1)" );
                    END ;
               OR
                    ACCEPT  E2 ;
                    GOTO  L123 ;
                    FAILED( "'GOTO' NOT OBEYED (1)" );
               OR
                    DELAY 10.0 * Impdef.One_Second;
                    FAILED( "DELAY ALTERNATIVE SELECTED (1)" );
               END SELECT;

               FAILED( "WRONG DESTINATION FOR 'GOTO' (1)" );

               << L123 >>

               FLOW_STRING(INDEX) := 'A' ;
               INDEX              := INDEX + 1 ;

          END T;

     BEGIN

          T.E2 ;

     END;

     -------------------------------------------------------------------

     DECLARE

          TASK  T  IS
               ENTRY  E1 ;
               ENTRY  E2 ;
          END  T ;

          TASK BODY T IS
          BEGIN

               SELECT 
                    ACCEPT  E1  DO
                         FAILED( " E1  ACCEPTED; NO ENTRY CALL (2)" );
                    END ;
               OR
                    ACCEPT  E2  DO
                         FAILED( " E2  ACCEPTED; NO ENTRY CALL (2)" );
                    END ;
               OR
                    DELAY 10.0 * Impdef.One_Second;
                    GOTO  L321 ;
                    FAILED( "'GOTO' NOT OBEYED (2)" );
               END SELECT;

               FAILED( "WRONG DESTINATION FOR 'GOTO' (2)" );

               << L321 >>

               FLOW_STRING(INDEX) := 'B' ;
               INDEX              := INDEX + 1 ;

          END T;

     BEGIN

          NULL ;

     END;

     -------------------------------------------------------------------

     IF  FLOW_STRING /= "AB"  THEN
          FAILED("WRONG FLOW OF CONTROL" );
     END IF;


     RESULT ;


END  C59002C ;
