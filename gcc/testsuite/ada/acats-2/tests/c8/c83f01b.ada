-- C83F01B.ADA

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
-- CHECK THAT INSIDE A PACKAGE BODY NESTED WITHIN ANOTHER PACKAGE BODY
--    AN ATTEMPT TO REFERENCE AN IDENTIFIER DECLARED IN THE
--    CORRESPONDING PACKAGE SPECIFICATION
--    IS SUCCESSFUL EVEN IF THE SAME IDENTIFIER IS DECLARED IN THE
--    OUTER PACKAGE (SPECIFICATION OR BODY)  OR  IN THE
--    ENVIRONMENT SURROUNDING THE OUTER PACKAGE BODY.

-- INTERACTIONS WITH SEPARATE COMPILATION ARE TESTED IN  C83F01C ,
--    C83F01D .


--    RM    08 AUGUST 1980
--    JRK   13 NOV    1980


WITH REPORT;
PROCEDURE  C83F01B  IS

     USE REPORT;

     X1 , X2 : INTEGER RANGE 1..23 := 17 ;

     TYPE  T1  IS  ( A , B , C) ;

     Z : T1 := A ;


BEGIN

     TEST( "C83F01B" , "CHECK THAT INSIDE A NESTED PACKAGE BODY" &
                       " AN ATTEMPT TO REFERENCE AN IDENTIFIER" &
                       " DECLARED IN THE CORRESPONDING PACKAGE SPECI" &
                       "FICATION  IS SUCCESSFUL EVEN IF THE SAME IDEN" &
                       "TIFIER IS DECLARED IN THE ENVIRONMENT SURROUND"&
                       "ING THE PACKAGE BODY" ) ;

     COMMENT("SEPARATELY COMPILED PACKAGES ARE TESTED IN  C83F01C, -D");


     DECLARE


          Y1 , Y2 : INTEGER := 100 ;


          PACKAGE  OUTER  IS

               Y3 : INTEGER := 100 ;

               PACKAGE  P  IS

                    X1 : BOOLEAN := FALSE ;
                    X2 : INTEGER RANGE 1..23 := 11 ;
                    Y1 , Y3 : BOOLEAN := TRUE ;
                    Y2 , Y4 : INTEGER := 5 ;
                    T1 : INTEGER := 6 ;
                    Z  : INTEGER := 7 ;

               END  P ;

          END  OUTER ;


          X2 : INTEGER := 100 ;


          PACKAGE BODY  OUTER  IS

               Y4 : INTEGER := 200 ;

               PACKAGE BODY  P  IS
               BEGIN

                    X1 :=  NOT X1  AND  Y1  AND  Y3 ;
                    Z  :=  Z + T1 ;
                    Y2 := X2 * Y2 ;
                    Y4 := X2 * Y4 ;

                    -- INCORRECT INTERPRETATIONS IN THE FIRST TWO
                    --    ASSIGNMENTS  MANIFEST THEMSELVES AT
                    --    COMPILE TIME AS TYPE ERRORS

               END P ;

          END OUTER ;


     BEGIN

          IF  X1 /= 17   OR
              Z  /= A    OR
              Y2 /= 100  OR
              NOT OUTER.P.X1    OR
              OUTER.P.Z  /= 13  OR
              OUTER.P.Y2 /= 55  OR
              OUTER.P.Y4 /= 55
          THEN FAILED( "INCORRECT ACCESSING" );
          END IF;

     END ;


     RESULT;   --  POSSIBLE ERROR DURING ELABORATION OF  P

END C83F01B;
