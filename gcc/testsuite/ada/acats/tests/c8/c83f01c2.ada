-- C83F01C2M.ADA

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
-- MAIN PROGRAM REQUIRING A SEPARATELY COMPILED PACKAGE
--    ( C83F01C0 ; SPECIFICATION IN  C83F01C0.ADA ,
--                 BODY IN           C83F01C1.ADA )

-- CHECK THAT INSIDE A PACKAGE BODY NESTED WITHIN A SEPARATELY COMPILED
--    PACKAGE BODY AN ATTEMPT TO REFERENCE AN IDENTIFIER DECLARED IN THE
--    CORRESPONDING PACKAGE SPECIFICATION
--    IS SUCCESSFUL EVEN IF THE SAME IDENTIFIER IS DECLARED IN THE
--    OUTER PACKAGE (SPECIFICATION OR BODY).

-- CASE 1:  PACKAGE IS A FULL-FLEDGED COMPILATION UNIT


--    RM    11 AUGUST 1980
--    RM    22 AUGUST 1980
--    RM    29 AUGUST 1980 (MOVED 'FAILED(.)' FROM C83F01C1.ADA TO HERE)


WITH REPORT , C83F01C0 ;
PROCEDURE  C83F01C2M  IS

     USE  REPORT , C83F01C0 ;

BEGIN

     TEST( "C83F01C" , "CHECK THAT INSIDE A  PACKAGE BODY" &
                       " NESTED WITHIN A SEPARATELY" &
                       " COMPILED PACKAGE BODY LIBRARY UNIT," &
                       " AN ATTEMPT TO REFERENCE AN IDENTIFIER" &
                       " DECLARED IN THE CORRESPONDING PACKAGE SPECI" &
                       "FICATION  IS SUCCESSFUL EVEN IF THE SAME IDEN" &
                       "TIFIER IS DECLARED IN THE OUTER PACKAGE" &
                       " (SPECIFICATION OR BODY)" ) ;

     IF  NOT P.X1    OR
         P.Z  /= 13  OR
         P.Y2 /= 55  OR
         P.Y4 /= 55
     THEN FAILED( "INCORRECT ACCESSING" );
     END IF;

     RESULT ;


END C83F01C2M;
