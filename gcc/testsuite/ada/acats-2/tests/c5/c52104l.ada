-- C52104L.ADA

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
-- OBJECTIVE:
--     CHECK THAT LENGTHS MUST MATCH IN ARRAY AND SLICE ASSIGNMENTS.
--     MORE SPECIFICALLY, TEST THAT ATTEMPTED ASSIGNMENTS BETWEEN
--     ARRAYS WITH NON-MATCHING LENGTHS LEAVE THE DESTINATION ARRAY
--     INTACT AND CAUSE  CONSTRAINT_ERROR  TO BE RAISED.
--     (OVERLAPS BETWEEN THE OPERANDS OF THE ASSIGNMENT STATEMENT
--     ARE TREATED ELSEWHERE.)

--     THIS IS THE SECOND FILE IN
--     DIVISION  C :  NON-NULL LENGTHS NOT DETERMINABLE STATICALLY.

-- HISTORY:
--     RM  07/20/81 CREATED ORIGINAL TEST.
--     SPS 03/22/83
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT;
PROCEDURE  C52104L  IS

     USE  REPORT ;

BEGIN

     TEST( "C52104L" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
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

          ARRX31  :  TABOX3( IDENT_INT(2)..IDENT_INT(6) )  := "QUINC" ;

     BEGIN


          -- ARRAY ASSIGNMENT (WITH STRING AGGREGATE):

          ARRX31 :=  "ABCD" ;
          FAILED( "NO EXCEPTION RAISED  (13)" );

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE ASSIGNMENT:

               IF  ARRX31                               /=  "QUINC"  OR
                   ARRX31( IDENT_INT(2)..IDENT_INT(6) ) /=  "QUINC"
               THEN
                    FAILED( "LHS ARRAY ALTERED  (13)" );
               END IF;

          WHEN  OTHERS  =>
               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 13" );

     END ;


     -------------------------------------------------------------------

     --   (14) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .

     DECLARE

          TYPE  TABOX4  IS  ARRAY( INTEGER RANGE <> )  OF CHARACTER ;

          SUBTYPE  TABOX42  IS  TABOX4( IDENT_INT(5)..IDENT_INT(9) );

          ARRX42  :  TABOX42 ;

     BEGIN

          -- INITIALIZATION OF LHS ARRAY:

          ARRX42  :=  "QUINC"  ;


          -- SLICE ASSIGNMENT:

          ARRX42( IDENT_INT(6)..IDENT_INT(9) )  :=  "ABCDEFGH"  ;
          FAILED( "NO EXCEPTION RAISED  (14)" );

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE ASSIGNMENT:

               IF  ARRX42                               /=  "QUINC"  OR
                   ARRX42( IDENT_INT(5)..IDENT_INT(9) ) /=  "QUINC"
               THEN
                    FAILED( "LHS ARRAY ALTERED  (14)" );
               END IF;

          WHEN  OTHERS  =>
               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 14" );

     END ;


     -------------------------------------------------------------------


     RESULT ;


END C52104L;
