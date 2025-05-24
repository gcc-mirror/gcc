-- C59002A.ADA

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
-- CHECK THAT JUMPS OUT OF AN EXCEPTION HANDLER CONTAINED IN A BLOCK
--    TO A STATEMENT IN AN ENCLOSING UNIT ARE ALLOWED AND ARE PERFORMED 
--    CORRECTLY.


-- RM 05/22/81
-- SPS 3/8/83

WITH REPORT;
PROCEDURE  C59002A  IS

     USE  REPORT ;

BEGIN

     TEST( "C59002A" , "CHECK THAT JUMPS OUT OF EXCEPTION HANDLERS" &
                       " ARE ALLOWED" );

     DECLARE

          FLOW : INTEGER := 1 ;
          EXPON: INTEGER RANGE 0..3 := 0 ;

     BEGIN

          GOTO  START ;

          FAILED( "'GOTO' NOT OBEYED" );

          << BACK_LABEL >>
          FLOW  := FLOW  * 3**EXPON ;                    -- 1*5*9
          EXPON := EXPON + 1 ;
          GOTO  FINISH ;

          << START >>
          FLOW  := FLOW  * 7**EXPON ;                    -- 1
          EXPON := EXPON + 1 ;

          DECLARE
          BEGIN
               RAISE  CONSTRAINT_ERROR ;
               FAILED( "EXCEPTION NOT RAISED  -  1" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    GOTO  FORWARD_LABEL ;
          END ;

          FAILED( "INNER 'GOTO' NOT OBEYED  -  1" );

          << FORWARD_LABEL >>
          FLOW  := FLOW  * 5**EXPON ;                    -- 1*5
          EXPON := EXPON + 1 ;

          DECLARE
          BEGIN
               RAISE  CONSTRAINT_ERROR ;
               FAILED( "EXCEPTION NOT RAISED  -  2" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR  =>
                    GOTO  BACK_LABEL ;
          END ;

          FAILED( "INNER 'GOTO' NOT OBETED  -  2" );

          << FINISH >>
          FLOW  := FLOW  * 2**EXPON ;                    -- 1*5*9*8

          IF  FLOW /= 360  THEN
               FAILED( "WRONG FLOW OF CONTROL" );
          END IF;

     END ;


     RESULT ;


END C59002A;
