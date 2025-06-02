-- C52104M.ADA

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
--    MORE SPECIFICALLY, TEST THAT ATTEMPTED ASSIGNMENTS BETWEEN
--    ARRAYS WITH NON-MATCHING LENGTHS LEAVE THE DESTINATION ARRAY
--    INTACT AND CAUSE  CONSTRAINT_ERROR  TO BE RAISED.
--    (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--    ARE TREATED ELSEWHERE.)

-- THIS IS THE THIRD FILE IN
--    DIVISION  C :  NON-NULL LENGTHS NOT DETERMINABLE STATICALLY.


-- RM 07/20/81
-- SPS 3/22/83

WITH REPORT;
PROCEDURE  C52104M  IS

     USE  REPORT ;

BEGIN

     TEST( "C52104M" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
                       " ASSIGNMENTS  THE LENGTHS MUST MATCH" );


     --                              ( EACH DIVISION COMPRISES 3 FILES,
     --                                COVERING RESPECTIVELY THE FIRST
     --                                3 , NEXT 2 , AND LAST 3 OF THE 8
     --                                SELECTIONS FOR THE DIVISION.)


     -------------------------------------------------------------------

     --    (7) UNSLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
     --        THEMSELVES).

     DECLARE

          ARR71  :  STRING( IDENT_INT(1)..IDENT_INT(5) )  := "ABCDE" ;
          ARR72  :  STRING( IDENT_INT(5)..IDENT_INT(8) )  := "FGHI"  ;

     BEGIN


          -- STRING ASSIGNMENT:

          ARR72 := ARR71 ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 7" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE STRING ASSIGNMENT:

               IF  ARR72 /= "FGHI"
               THEN
                    FAILED( "ORIGINAL VALUE ALTERED (7)" );
               END IF;

          WHEN  OTHERS  =>
               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 7" );

     END ;


     -------------------------------------------------------------------


     --    (8) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING' , WITH
     --        STRING LITERALS.
     --

     DECLARE

          ARR82 : STRING( IDENT_INT(5)..IDENT_INT(9) )  :=  "QBCDE"  ;

     BEGIN


          -- STRING LITERAL ASSIGNMENT:

          ARR82( IDENT_INT(5)..IDENT_INT(9) )
               ( IDENT_INT(6)..IDENT_INT(9) ) :=  "EIN" ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 8" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

               IF  ARR82                               /=  "QBCDE"  OR
                   ARR82( IDENT_INT(5)..IDENT_INT(9) ) /=  "QBCDE"
               THEN
                    FAILED( "LHS ARRAY ALTERED  (8)" );
               END IF;

          WHEN  OTHERS  =>
               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 8" );

     END ;


     -------------------------------------------------------------------

     --    (9) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
     --        THEMSELVES).
     --

     DECLARE

          SUBTYPE  TA92  IS  STRING( IDENT_INT(5)..IDENT_INT(9) ) ;

          ARR91  :  STRING( IDENT_INT(1)..IDENT_INT(7) )  := "ABCDEFG" ;
          ARR92  :  TA92 ;

     BEGIN


          -- INITIALIZATION OF LHS ARRAY:

          ARR92( IDENT_INT(5)..IDENT_INT(9) )  :=  "QUINC"  ;


          -- STRING SLICE ASSIGNMENT:

          ARR92( IDENT_INT(5)..IDENT_INT(9) )
               ( IDENT_INT(6)..IDENT_INT(9) ) :=
               ARR91
               ( IDENT_INT(1)..IDENT_INT(7) )
               ( IDENT_INT(1)..IDENT_INT(6) )
               ( IDENT_INT(1)..IDENT_INT(6) ) ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 9" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

               IF  ARR92                               /=  "QUINC"  OR
                   ARR92( IDENT_INT(5)..IDENT_INT(9) ) /=  "QUINC"
               THEN
                    FAILED( "LHS VALUE ALTERED  (9)" );
               END IF;

          WHEN  OTHERS  =>
               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 9" );

     END ;


     -------------------------------------------------------------------


     RESULT ;


END C52104M;
