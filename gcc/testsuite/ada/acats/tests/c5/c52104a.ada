-- C52104A.ADA

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

--    DIVISION  A : STATICALLY-DETERMINABLE NON-NULL LENGTHS.


-- RM 07/20/81
-- SPS 3/22/83

WITH REPORT;
PROCEDURE  C52104A  IS

     USE  REPORT ;

BEGIN

     TEST( "C52104A" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
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
     --    (-) SPECIAL CASES:  NULL ARRAYS....... TREATED IN DIVISION B.
     --                        SUPERLONG ARRAYS.. (TREATED FOR DYNAMIC
     --                                            ARRAYS ONLY,
     --                                            DIVISIONS C AND D .)
     --
     --
     --    (-) THE DYNAMIC-ARRAY COUNTERPARTS OF THESE TESTS ARE IN DI-
     --        VISIONS C (FOR NON-NULL ARRAYS) AND D (FOR NULL ARRAYS).
     --
     --


     -------------------------------------------------------------------

     --    (1..6: NOT APPLICABLE)
     --
     --

     -------------------------------------------------------------------

     --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
     --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
     --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

     DECLARE

          TYPE  TABOX0  IS  ARRAY( INTEGER RANGE <> , INTEGER RANGE <>
                                 )  OF INTEGER ;

          SUBTYPE  TABOX01  IS  TABOX0( 1..5 ,  0..7  );
          SUBTYPE  TABOX02  IS  TABOX0( 0..5 ,  2..9  );

          ARRX01  :  TABOX01 ;
          ARRX02  :  TABOX02 ;

     BEGIN

          -- INITIALIZATION OF RHS ARRAY:

          FOR  I  IN  1..5  LOOP

               FOR  J  IN  0..7  LOOP
                    ARRX01( I , J )  :=   I * I * J  ;
               END LOOP;

          END LOOP;


          -- INITIALIZATION OF LHS ARRAY:

          FOR  I  IN  0..5  LOOP

               FOR  J  IN  2..9  LOOP
                    ARRX02( I , J )  :=   I * I * J * 3  ;
               END LOOP;

          END LOOP;


          -- ARRAY ASSIGNMENT:

          ARRX02 := ARRX01 ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 10" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE ARRAY ASSIGNMENT:

               FOR  I  IN  0..5  LOOP

                    FOR  J  IN  2..9  LOOP

                         IF  ARRX02( I , J )  /=   I * I * J * 3
                         THEN
                              FAILED( "ORIG. VALUE ALTERED (10)" );
                         END IF;

                    END LOOP;

               END LOOP;

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

          SUBTYPE  TABOX11  IS  TABOX1( 1..5 ) ;

          ARRX11  :  TABOX11 ;
          ARRX12  :  TABOX1( 6..9 );

     BEGIN

          -- INITIALIZATION OF RHS ARRAY:

          FOR  I  IN  1..5  LOOP

               ARRX11( I )  :=   I * I  ;

          END LOOP;


          -- INITIALIZATION OF LHS ARRAY:

          FOR  I  IN  6..9  LOOP
               ARRX12( I )  :=   I * I * 3  ;
          END LOOP;


          -- ARRAY ASSIGNMENT:

          ARRX12 := ARRX11 ;
          FAILED( "EXCEPTION NOT RAISED  -  SUBTEST 11" );

     EXCEPTION

          WHEN  CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE ARRAY ASSIGNMENT:

               FOR  I  IN  6..9  LOOP

                    IF  ARRX12( I )  /=   I * I * 3
                    THEN
                         FAILED( "ORIG. VALUE ALTERED (11)" );
                    END IF;

               END LOOP;

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

          -- INITIALIZATION OF LHS ARRAY:

          FOR  I  IN  5..9  LOOP
               ARRX52( I )  :=  FALSE  ;
          END LOOP;


          -- INITIALIZATION OF RHS ARRAY:

          FOR  I  IN  1..5  LOOP
               ARRX51( I )  :=  TRUE  ;
          END LOOP;


          -- SLICE ASSIGNMENT:

          ARRX52(6..9) := ARRX51(3..3) ;
          FAILED( "EXCEPTION NOT RAISED  (12)" );

     EXCEPTION

          WHEN CONSTRAINT_ERROR =>

               -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

               FOR  I  IN  5..9  LOOP

                    IF  ARRX52( I )  /=  FALSE
                    THEN
                         FAILED( "LHS ARRAY ALTERED  ( 12 ) " );
                    END IF;

               END LOOP;

          WHEN  OTHERS  =>
               FAILED( "EXCEPTION RAISED  -  SUBTEST 12" );

     END ;


     -------------------------------------------------------------------


     RESULT ;


END C52104A;
