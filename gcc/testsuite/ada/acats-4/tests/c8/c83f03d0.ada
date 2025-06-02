-- C83F03D0M.ADA

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
-- MAIN PROGRAM REQUIRING A SEPARATELY COMPILED PACKAGE BODY SUBUNIT
--    ( C83F03D1.ADA )

-- CHECK THAT IF A PACKAGE BODY IS NESTED INSIDE A SEPARATELY COMPILED
--    PACKAGE BODY
--    THE INNER PACKAGE BODY CAN CONTAIN A LABEL IDENTIFIER IDENTICAL
--    TO A LABEL IDENTIFIER IN THE OUTER PACKAGE BODY OR TO AN IDENTI-
--    FIER DECLARED IN THE OUTER PACKAGE BODY OR IN ITS SPECIFICATION
--    OR IN ITS ENVIRONMENT.

-- CASE 2:  PACKAGE BODY IS A COMPILATION SUBUNIT


--    RM    08 SEPTEMBER 1980
--    JRK   14 NOVEMBER  1980


WITH REPORT;
PROCEDURE  C83F03D0M  IS

     USE REPORT ;

     X1 : INTEGER := 17 ;

     TYPE  T1  IS  ( A, B, C ) ;

     Z : T1 := A ;

     FLOW_INDEX : INTEGER := 0 ;


     PACKAGE  C83F03D1  IS

          Y3 : INTEGER := 100 ;

          TYPE  T3  IS  ( D , E , F ) ;

          PACKAGE  P  IS
               AA : BOOLEAN := FALSE ;
          END  P ;

     END  C83F03D1 ;


     Y1 : INTEGER := 100 ;


     PACKAGE BODY  C83F03D1  IS SEPARATE ;


BEGIN

     TEST( "C83F03D" , "CHECK THE RECOGNITION OF LABELS IN NESTED" &
                       " PACKAGES SEPARATELY COMPILED AS SUBUNITS" );

     << LABEL_IN_MAIN >>

     IF  FLOW_INDEX /= 10
     THEN  FAILED( "INCORRECT FLOW OF CONTROL" );
     END IF;

     RESULT;   --  POSS. ERROR DURING ELABORATION OF  P


END C83F03D0M;
