-- C83F03A.ADA

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
-- CHECK THAT INSIDE A PACKAGE BODY  AN ATTEMPT TO PLACE AND REFERENCE
--    A LABEL IS SUCCESSFUL EVEN IF ITS IDENTIFIER IS DECLARED IN THE
--    ENVIRONMENT SURROUNDING THE PACKAGE BODY.

-- NESTED PACKAGE BODIES ARE TESTED IN  C83F03B , C83F03C , C83F03D


--    RM    03 SEPTEMBER 1980


WITH REPORT;
PROCEDURE  C83F03A  IS

     USE REPORT;

     X1 , X2 : INTEGER RANGE 1..23 := 17 ;

     TYPE  T1  IS  ( A , B , C) ;

     Z : T1 := A ;

     FLOW_INDEX : INTEGER := 0 ;

BEGIN

     TEST( "C83F03A" , "CHECK THAT INSIDE A PACKAGE BODY " &
                       " AN ATTEMPT TO PLACE AND REFERENCE A LABEL" &
                       " IS SUCCESSFUL EVEN IF ITS IDEN" &
                       "TIFIER IS DECLARED IN THE ENVIRONMENT SURROUND"&
                       "ING THE PACKAGE BODY" ) ;


     DECLARE


          Y1 , Y2 : INTEGER := 13 ;


          PROCEDURE  BUMP  IS
          BEGIN
               FLOW_INDEX := FLOW_INDEX + 1 ;
          END BUMP ;


          PACKAGE  P  IS

               AA : BOOLEAN := FALSE ;

          END  P ;


          PACKAGE BODY  P  IS
          BEGIN

               GOTO  X1 ;

               BUMP ;
               BUMP ;

               <<X1>>   BUMP ;  GOTO  X2 ;
               BUMP ;
               <<T1>>   BUMP ;  GOTO  Z ;
               BUMP ;
               <<Y1>>   BUMP ;  GOTO  Y2 ;
               BUMP ;
               <<Y2>>   BUMP ;  GOTO  T1 ;
               BUMP ;
               <<X2>>   BUMP ;  GOTO  Y1 ;
               BUMP ;
               <<Z >>   BUMP ;  GOTO  ENDING ;
               BUMP ;

               << ENDING >>  NULL;

          END P ;


     BEGIN

          IF  FLOW_INDEX /= 6
          THEN  FAILED( "INCORRECT FLOW OF CONTROL" );
          END IF;

     END ;


     RESULT;   --  POSS. ERROR DURING ELABORATION OF  P

END C83F03A;
