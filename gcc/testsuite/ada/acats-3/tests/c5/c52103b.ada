-- C52103B.ADA

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
-- CHECK THAT LENGTHS MUST MATCH IN ARRAY AND SLICE ASSIGNMENTS.
--    MORE SPECIFICALLY, TEST THAT ARRAY ASSIGNMENTS WITH MATCHING
--    LENGTHS DO NOT CAUSE  CONSTRAINT_ERROR  TO BE RAISED AND
--    ARE PERFORMED CORRECTLY.
--    (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--    ARE TREATED ELSEWHERE.)

-- THIS IS THE SECOND FILE IN
--    DIVISION  A : STATICALLY-DETERMINABLE NON-NULL LENGTHS.


-- RM 07/20/81
-- SPS 2/18/83

WITH REPORT;
PROCEDURE  C52103B  IS

     USE  REPORT ;

BEGIN

     TEST( "C52103B" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
                       " ASSIGNMENTS  THE LENGTHS MUST MATCH" );


     --                              ( EACH DIVISION COMPRISES 3 FILES,
     --                                COVERING RESPECTIVELY THE FIRST
     --                                3 , NEXT 2 , AND LAST 3 OF THE 8
     --                                SELECTIONS FOR THE DIVISION.)


     -------------------------------------------------------------------

     --   (13) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .

     DECLARE

          TYPE  TABOX3  IS  ARRAY( INTEGER RANGE <> )  OF CHARACTER ;

          ARRX31  :  TABOX3( 11..15 );

     BEGIN


          -- ARRAY ASSIGNMENT (WITH STRING AGGREGATE):

          ARRX31 :=  "QUINC" ; -- "QUINC"(1..5)  SLIDES TO 11..15


          -- CHECKING THE VALUES AFTER THE ASSIGNMENT:

          IF  ARRX31           /=  "QUINC"  OR
              ARRX31( 11..15 ) /=  "QUINC"
          THEN
               FAILED( "ARRAY ASSIGNMENT NOT CORRECT (13)" );
          END IF;

     EXCEPTION

          WHEN  OTHERS  =>
               FAILED( "EXCEPTION RAISED  -  SUBTEST 13" );

     END ;


     -------------------------------------------------------------------

     --    (6) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
     --

     DECLARE

          TYPE  TA61  IS  ARRAY( INTEGER RANGE 11..15 )  OF CHARACTER ;

          ARR61  :  TA61 ;

     BEGIN

          -- INITIALIZATION OF UNUSED COMPONENT OF LHS ARRAY:

          ARR61( 11..11 )  :=  "Q"  ;


          -- SLICE ASSIGNMENT:

          ARR61( 12..15 ) :=  "UINC" ; -- "UINC"(1..4)  SLIDES TO 12..15


          -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

          IF  ARR61           /=  "QUINC"  OR
              ARR61( 11..15 ) /=  "QUINC"
          THEN
               FAILED( "SLICE ASSIGNMENT NOT CORRECT (6)" );
          END IF;

     EXCEPTION

          WHEN  OTHERS  =>
               FAILED( "EXCEPTION RAISED  -  SUBTEST 6" );

     END ;


     -------------------------------------------------------------------


     RESULT ;


END C52103B;
