-- C52104F.ADA

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
--    ARE TREATED ELSWEWHERE.)

--    DIVISION  B : STATICALLY-DETERMINABLE NULL LENGTHS.


-- RM 07/20/81
-- SPS 10/27/82

WITH REPORT;
PROCEDURE  C52104F  IS

     USE  REPORT ;

BEGIN

     TEST( "C52104F" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
                       " ASSIGNMENTS  THE LENGTHS MUST MATCH" );


     -- IN THIS TEST WE CAN'T USE AGGREGATE ASSIGNMENT (EXCEPT WHEN
     --    THE AGGREGATES ARE STRING LITERALS); THEREFORE:
     --
     --    (1) ARRAYS WILL BE INITIALIZED BY INDIVIDUAL ASSIGNMENTS;
     --    (2) CAN'T USE NON-NULL CONSTANT ARRAYS.


     -- WE ASSUME THAT IN AN ARRAY_TYPE_DEFINITION THE INDEX PORTION
     --    AND THE COMPONENT_TYPE PORTION ARE FUNCTIONALLY ORTHOGONAL
     --    ALSO AT THE IMPLEMENTATION LEVEL, I.E. THAT THE CORRECTNESS
     --    OF THE ACCESSING MECHANISM FOR ARRAYS DOES NOT DEPEND ON
     --    COMPONENT_TYPE.  ACCORDINGLY WE ARE TESTING FOR SOME BUT
     --    NOT ALL KINDS OF COMPONENT_TYPE.  (COMPONENT_TYPES INCLUDED:
     --    INTEGER , CHARACTER , BOOLEAN .)


     -- CASES DISTINGUISHED:         ( 8 SELECTED CASES ARE IMPLEMENTED)
     --
     --                              ( THE 8 SELECTIONS ARE THE 5-CASE
     --                                SERIES 10-11-12-13-14 FOLLOWED
     --                                BY  7 , 8 , 9 (IN THIS ORDER). )
     --
     --
     --                              ( EACH DIVISION COMPRISES 3 FILES,
     --                                COVERING RESPECTIVELY THE FIRST
     --                                3 , NEXT 2 , AND LAST 3 OF THE 8
     --                                SELECTIONS FOR THE DIVISION.)
     --
     --
     --    (1..6) (DO NOT APPLY TO NON-MATCHING OBJECTS, SINCE WE WANT
     --        THE OBJECTS TO HAVE THE   S A M E   BASE TYPE.)
     --
     --
     --    (7) UNSLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
     --        THEMSELVES).
     --
     --
     --    (8) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING' , WITH
     --        STRING LITERALS.
     --
     --
     --    (9) SLICED OBJECTS OF THE PREDEFINED TYPE  'STRING'  (BY
     --        THEMSELVES).
     --
     --
     --    (-) CONSTRAINABLE TYPES:  ONLY SUBTESTS   2,  3,  4,  5,  6
     --        WILL BE REPLICATED  --  AS SUBTESTS  10, 11, 12, 13, 14 .
     --
     --
     --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
     --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
     --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)
     --
     --
     --   (11) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)
     --
     --
     --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)
     --
     --
     --   (13) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
     --
     --        (STRING LITERALS ARE THE ONLY AGGREGATES WE ARE USING
     --        IN THIS TEST.  TO FORCE SLIDING, THE LOWER LIMIT IMPLIED
     --        BY THE TYPEMARK WILL NOT BE  1 .)
     --
     --
     --   (14) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
     --
     --
     --
     --    (-) SPECIAL CASES:  SUPERLONG ARRAYS.. (TREATED FOR DYNAMIC
     --                                            ARRAYS ONLY,
     --                                            DIVISIONS C AND D .)
     --
     --
     --    (-) THE DYNAMIC-ARRAY COUNTERPARTS OF THESE TESTS ARE IN DI-
     --        VISIONS C (FOR NON-NULL ARRAYS) AND D (FOR NULL ARRAYS).
     --
     --


     -------------------------------------------------------------------

     --    (1 .. 6: NOT APPLICABLE)
     --
     --

     -------------------------------------------------------------------

     --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
     --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
     --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

     DECLARE

          TYPE  TABOX0  IS  ARRAY( INTEGER RANGE <> , INTEGER RANGE <>
                                 )  OF INTEGER ;

          SUBTYPE  TABOX01  IS  TABOX0( 1..1 ,  0..7  );
          SUBTYPE  TABOX02  IS  TABOX0 ;

          ARRX01  :  TABOX01 ;
          ARRX02  :  TABOX02( 1..0 , 0..7 );

     BEGIN

          -- ARRAY ASSIGNMENT:

          ARRX02 := ARRX01 ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 10" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               NULL ;

          WHEN  OTHERS  =>

               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 10" );

     END ;


     -------------------------------------------------------------------

     --   (11) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)

     DECLARE

          TYPE  TABOX1  IS  ARRAY( INTEGER RANGE <> )  OF INTEGER ;

          SUBTYPE  TABOX11  IS  TABOX1( 4..5 ) ;

          ARRX11  :  TABOX11 ;
          ARRX12  :  TABOX1( 5..4 );

     BEGIN

          -- ARRAY ASSIGNMENT:

          ARRX12 := ARRX11 ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 11" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               NULL ;

          WHEN  OTHERS  =>

               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 11" );

     END ;


     -------------------------------------------------------------------

     --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

     DECLARE

          TYPE  TABOX5  IS  ARRAY( INTEGER RANGE <> )  OF BOOLEAN ;

          SUBTYPE  TABOX51  IS  TABOX5( 1..5 );

          ARRX51  :  TABOX51 ;
          ARRX52  :  TABOX5( 5..9 );

     BEGIN

          -- INITIALIZATION OF RHS ARRAY:

          FOR  I  IN  1..5  LOOP
               ARRX51( I )  :=  FALSE  ; -- VALUES WILL BE:  F T F F T
          END LOOP;

          ARRX51(2) := TRUE ;

          ARRX51(5) := TRUE ;            -- RHS VALUES ARE:  F T F F T


          -- INITIALIZATION OF LHS ARRAY:

          FOR  I  IN  5..9  LOOP
               ARRX52( I )  :=  TRUE   ; -- VALUES WILL BE:  T F T T F
          END LOOP;

          ARRX52(6) := FALSE ;

          ARRX52(9) := FALSE ;           -- LHS VALUES ARE:  T F T T F


          -- NULL SLICE ASSIGNMENT:

          ARRX52( 6..5 ) := ARRX51( 4..4 ) ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 12" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:
               IF  ARRX52( 5 )  /=  TRUE   OR
                   ARRX52( 6 )  /=  FALSE  OR
                   ARRX52( 7 )  /=  TRUE   OR
                   ARRX52( 8 )  /=  TRUE   OR
                   ARRX52( 9 )  /=  FALSE
               THEN
                    FAILED( "LHS ARRAY ALTERED  (12)" );
               END IF;

          WHEN  OTHERS =>

               FAILED( "WRONG EXCEPTION RAISED  -  SUBTEST 12" );

     END ;


     -------------------------------------------------------------------


     RESULT ;


END C52104F;
