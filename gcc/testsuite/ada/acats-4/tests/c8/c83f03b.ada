-- C83F03B.ADA

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
-- CHECK THAT IF A PACKAGE BODY IS NESTED INSIDE ANOTHER PACKAGE BODY
--    THE INNER PACKAGE BODY CAN CONTAIN A LABEL IDENTIFIER IDENTICAL
--    TO A LABEL IDENTIFIER IN THE OUTER PACKAGE BODY, TO AN IDENTI-
--    FIER DECLARED IN THE OUTER PACKAGE BODY OR IN ITS SPECIFICATION,
--    OR TO A LABEL IDENTIFIER OR OTHER IDENTIFIER IN THE
--    ENVIRONMENT SURROUNDING THE OUTER PACKAGE BODY.


-- INTERACTIONS WITH SEPARATE COMPILATION ARE TESTED IN  C83F03C ,
--    C83F03D .


--    RM    04 SEPTEMBER 1980


WITH REPORT;
PROCEDURE  C83F03B  IS

     USE REPORT;

     X1 , X2 : INTEGER RANGE 1..23 := 17 ;

     TYPE  T1  IS  ( A , B , C) ;

     Z : T1 := A ;

     FLOW_INDEX : INTEGER := 0 ;

BEGIN

     TEST( "C83F03B" , "CHECK THAT IF A PACKAGE BODY IS NESTED" &
                       " INSIDE ANOTHER PACKAGE BODY, THE INNER" &
                       " PACKAGE BODY CAN CONTAIN A LABEL IDENTIFIER" &
                       " IDENTICAL TO A LABEL IDENTIFIER IN THE OUTER" &
                       " PACKAGE BODY, TO AN IDENTIFIER DECLARED IN" &
                       " THE OUTER PACKAGE BODY OR IN ITS SPECIFICA" &
                       "TION, OR TO A LABEL IDENTIFIER OR OTHER" &
                       " IDENTIFIER IN THE ENVIRONMENT SURROUNDING" &
                       " THE OUTER PACKAGE BODY" ) ;


     DECLARE


          Y1 , Y2 : INTEGER := 100 ;

          X2 : INTEGER := 100 ;


          PROCEDURE  BUMP  IS
          BEGIN
               FLOW_INDEX := FLOW_INDEX + 1 ;
          END BUMP ;


          PACKAGE  OUTER  IS

               Y3 : INTEGER := 100 ;

               TYPE  T3  IS  ( D , E , F ) ;

               PACKAGE  P  IS
                    AA : BOOLEAN := FALSE ;
               END  P ;

          END  OUTER ;


          PACKAGE BODY  OUTER  IS

               Y4 : INTEGER := 200 ;

               TYPE  T4  IS  ( G , H , I ) ;

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
                    <<Z >>   BUMP ;  GOTO  T3 ;
                    BUMP ;
                    <<T3>>   BUMP ;  GOTO  T4 ;
                    BUMP ;
                    <<LABEL_IN_OUTER>>   BUMP ;  GOTO  LABEL_IN_MAIN ;
                    BUMP ;
                    <<Y3>>   BUMP ;  GOTO  Y4 ;
                    BUMP ;
                    <<Y4>>   BUMP ;  GOTO  LABEL_IN_OUTER ;
                    BUMP ;
                    <<T4>>   BUMP ;  GOTO  Y3 ;
                    BUMP ;
                    <<LABEL_IN_MAIN >>   BUMP ;  GOTO  ENDING ;
                    BUMP ;

                    << ENDING >>  NULL;

               END P ;

          BEGIN

               << LABEL_IN_OUTER >>  NULL ;

          END OUTER ;


     BEGIN

          << LABEL_IN_MAIN >>

          IF  FLOW_INDEX /= 12
          THEN  FAILED( "INCORRECT FLOW OF CONTROL" );
          END IF;

     END ;


     RESULT;   --  POSS. ERROR DURING ELABORATION OF  P

END C83F03B;
