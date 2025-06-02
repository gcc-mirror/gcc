-- C83F01D0M.ADA

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
--    ( C83F01D1.ADA )

-- CHECK THAT INSIDE A PACKAGE BODY NESTED WITHIN A SEPARATELY COMPILED
--    PACKAGE BODY AN ATTEMPT TO REFERENCE AN IDENTIFIER DECLARED IN THE
--    CORRESPONDING PACKAGE SPECIFICATION
--    IS SUCCESSFUL EVEN IF THE SAME IDENTIFIER IS DECLARED IN THE
--    OUTER PACKAGE (SPECIFICATION OR BODY).

-- CASE 2:  PACKAGE BODY IS A COMPILATION SUBUNIT


--    RM    13 AUGUST 1980
--    RM    29 AUGUST 1980
--    JRK   13 NOV    1980


WITH REPORT;
PROCEDURE  C83F01D0M  IS

     USE REPORT ;

     X1 , X2 : INTEGER RANGE 1..23 := 17 ;
     Y1      : INTEGER := 157 ;

     TYPE  T1  IS  ( A , B , C) ;

     Z : T1 := A ;


     PACKAGE  C83F01D1  IS

          Y3 : INTEGER := 100 ;

          PACKAGE  P  IS

               X1 : BOOLEAN := FALSE ;
               X2 : INTEGER RANGE 1..23 := 11 ;
               Y1 , Y3 : BOOLEAN := TRUE ;
               Y2 , Y4 : INTEGER := 5 ;
               T1 : INTEGER := 23 ;
               Z  : INTEGER := 0 ;

          END  P ;

     END  C83F01D1 ;


     Y2 : INTEGER := 200 ;


     PACKAGE BODY  C83F01D1  IS SEPARATE ;


BEGIN

     TEST( "C83F01D" , "CHECK THAT INSIDE A  PACKAGE BODY" &
                       " NESTED WITHIN A SEPARATELY" &
                       " COMPILED PACKAGE BODY SUBUNIT," &
                       " AN ATTEMPT TO REFERENCE AN IDENTIFIER" &
                       " DECLARED IN THE CORRESPONDING PACKAGE SPECI" &
                       "FICATION  IS SUCCESSFUL EVEN IF THE SAME IDEN" &
                       "TIFIER IS DECLARED IN THE OUTER PACKAGE" &
                       " (SPECIFICATION OR BODY)" ) ;

     IF  X1 /= 17   OR
         Z  /= A    OR
         Y2 /= 200  OR
         NOT C83F01D1.P.X1    OR
         C83F01D1.P.Z  /= 23  OR
         C83F01D1.P.Y2 /= 55  OR
         C83F01D1.P.Y4 /= 55
     THEN FAILED( "INCORRECT ACCESSING" );
     END IF;

     RESULT ;


END C83F01D0M;
