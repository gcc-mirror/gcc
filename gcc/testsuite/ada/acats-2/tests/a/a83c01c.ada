-- A83C01C.ADA

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
-- CHECK THAT COMPONENT NAMES MAY BE THE SAME AS NAMES OF
--    FORMAL PARAMETERS, LABELS, LOOP PARAMETERS,
--    VARIABLES, CONSTANTS, SUBPROGRAMS, PACKAGES, TYPES.
-- (NAMES OF COMPONENTS IN LOGICALLY NESTED RECORDS ARE TESTED IN
--    C83C01B.ADA .)
-- (NAMES OF TASKS ARE TESTED IN  A83C01T.ADA .)

--    RM    24 JUNE 1980
--    JRK   10 NOV  1980
--    RM    01 JAN  1982

WITH REPORT;
PROCEDURE  A83C01C  IS

     USE REPORT;

BEGIN

     TEST( "A83C01C" , "CHECK THAT COMPONENT NAMES MAY BE THE SAME AS" &
                       " NAMES OF VARIABLES AND CONSTANTS " ) ;



     DECLARE

          VAR1 , VAR2 : INTEGER := 27 ;
          CONST1      : CONSTANT INTEGER := 13 ;
          CONST2      : CONSTANT BOOLEAN := FALSE ;

          TYPE  R1A  IS
               RECORD
                    VAR1,VAR2,CONST1:INTEGER ;
               END RECORD ;

          TYPE  R1  IS
               RECORD
                    VAR1   : INTEGER ;
                    VAR2   : BOOLEAN ;
                    CONST1 : BOOLEAN ;
                    A      : R1A ;
               END RECORD ;

          A : R1 := ( VAR1 => VAR1 , A => ( VAR1 => VAR2 ,
                                            VAR2 => VAR2 ,
                                            CONST1 => VAR1 ) ,
                      VAR2 => CONST2 , CONST1 => CONST2 ) ;

     BEGIN

          VAR1 := A.A.VAR2 ;
          A.CONST1 := CONST2 ;
          A.A.CONST1 := A.VAR1 + VAR2 ;

     END ;


     RESULT;

END A83C01C;
