-- C52103P.ADA

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

--    DIVISION  D : NULL LENGTHS NOT DETERMINABLE STATICALLY.


-- RM 07/20/81
-- SPS 3/22/83


WITH REPORT;
PROCEDURE  C52103P  IS

     USE  REPORT ;

BEGIN

     TEST( "C52103P" , "CHECK THAT IN ARRAY ASSIGNMENTS AND IN SLICE" &
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
     --                              ( THE SELECTIONS ARE  7 , 8 , 9 ,
     --                                AND PRECISELY 5 CASES FROM THE
     --                                TWO 5-CASE SERIES 2-3-4-5-6 AND
     --                                                  10-11-12-13-14)
     --
     --                              ( IN THE CURRENT DIVISION, THE 5
     --                                FLOATING SELECTIONS ARE 10-3-12-
     --                                -5-14 ; THUS THE 8 SELECTIONS ARE
     --                                10-3-12-5-14-7-8-9 (IN THIS ORDER
     --                                ).)
     --
     --
     --                              ( EACH DIVISION COMPRISES 3 FILES,
     --                                COVERING RESPECTIVELY THE FIRST
     --                                3 , NEXT 2 , AND LAST 3 OF THE 8
     --                                SELECTIONS FOR THE DIVISION.)
     --
     --
     --    (1) ARRAY OBJECTS DECLARED IN THE SAME DECLARATION.
     --        (TWO-DIMENSIONAL; NON-CONSTRAINABLE TYPEMARK.)
     --
     --        (THIS WILL BE THE ONLY CASE INVOLVING OBJECTS DECLARED
     --        IN THE SAME DECLARATION.)
     --
     --
     --    (2) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
     --        DEFINED WITHOUT EVER USING THE "BOX" COMPOUND SYMBOL.
     --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)
     --
     --        (SLICING IS ILLEGAL; SINCE IN THIS TEST WE ARE NEVER
     --        USING AGGREGATES
     --                (EXCEPT FOR ONE-DIMENSIONAL ARRAYS OF CHARACTERS;
     --                SEE  (5) )
     --        AND WE ARE NOT USING CONVERSION-TO-CONSTRAINED-TYPEMARKS
     --                (AS IN    T1(ARR)   , WHERE  ARR  IS AN ARRAY
     --                OBJECT AND  T1  IS AN ARRAY TYPEMARK SIMILAR
     --                -- AS MORE PRECISELY SPECIFIED IN  RM 4.6(B) --
     --                TO THE TYPEMARK OF  ARR ),
     --        THE ARRAY ASSIGNMENT CANNOT INVOLVE ANY SLIDING,
     --        AND THE TYPEMARKS ARE ESSENTIALLY THE SAME.)
     --
     --
     --    (3) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)
     --
     --        (SINCE WE ARE NOT USING AGGREGATES
     --        AND WE ARE NOT USING CONVERSION-TO-CONSTRAINED-TYPEMARKS,
     --        THE ARRAY ASSIGNMENT CANNOT INVOLVE ANY SLIDING,
     --        AND THE TYPEMARKS ARE ESSENTIALLY THE SAME.)
     --
     --
     --    (4) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)
     --
     --        (THE ASSIGNMENT MAY REQUIRE SLIDING.)
     --
     --        (MOST SUBSEQUENT SUBCASES IN THIS TEST (OTHER THAN NULL
     --        ASSIGNMENTS) WILL INVOLVE SLIDING; WE ASSUME THAT
     --        SUBCASES WHICH WORK IN CONJUNCTION WITH SLIDING  WORK
     --        ALSO WHEN NO SLIDING IS INVOLVED.)
     --
     --
     --    (5) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
     --
     --        (STRING LITERALS ARE THE ONLY AGGREGATES WE ARE USING
     --        IN THIS TEST.  TO FORCE SLIDING, THE LOWER LIMIT IMPLIED
     --        BY THE TYPEMARK WILL NOT BE  1 .)
     --
     --
     --    (6) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS  'CHARACTER' .
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

     --   (10) MULTIDIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS WERE
     --        DEFINED USING THE "BOX" COMPOUND SYMBOL.
     --        (TWO-DIMENSIONAL ARRAYS OF INTEGERS.)

     DECLARE

          TYPE  TABOX0  IS  ARRAY( INTEGER RANGE <> , INTEGER RANGE <>
                                 )  OF INTEGER ;

          SUBTYPE  TABOX01  IS  TABOX0( IDENT_INT(1)..IDENT_INT(0) ,
                                        IDENT_INT(0)..IDENT_INT(7)  );
          SUBTYPE  TABOX02  IS  TABOX0 ;

          ARRX01  :  TABOX01 ;
          ARRX02  :  TABOX02( IDENT_INT(7)..IDENT_INT(6) ,
                              IDENT_INT(20)..IDENT_INT(27) );

     BEGIN

          -- ARRAY ASSIGNMENT:

          ARRX02 := ARRX01 ;

     EXCEPTION

          WHEN  OTHERS  =>
               FAILED( "EXCEPTION RAISED  -  SUBTEST 10" );

     END ;


     -------------------------------------------------------------------

     --    (3) UNSLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED WITHOUT EVER USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF INTEGERS.)

     DECLARE

          TYPE  TA3  IS  ARRAY(
               INTEGER RANGE IDENT_INT(100)..IDENT_INT(99)
                              )  OF INTEGER ;

          SUBTYPE  TA31  IS  TA3 ;
          SUBTYPE  TA32  IS  TA3 ;

          ARR31  :  TA31 ;
          ARR32  :  TA32 ;

     BEGIN

          -- ARRAY ASSIGNMENT:

          ARR32 := ARR31 ;

     EXCEPTION

          WHEN  OTHERS  =>
               FAILED( "EXCEPTION RAISED  -  SUBTEST 3" );

     END ;


     -------------------------------------------------------------------

     --   (12) SLICED ONE-DIMENSIONAL ARRAY OBJECTS WHOSE TYPEMARKS
     --        WERE DEFINED USING THE "BOX" SYMBOL
     --        AND FOR WHICH THE COMPONENT TYPE IS NOT  'CHARACTER' .
     --        ((ONE-DIMENSIONAL) ARRAYS OF BOOLEANS.)

     DECLARE

          TYPE  TABOX5  IS  ARRAY( INTEGER RANGE <> )  OF BOOLEAN ;

          SUBTYPE  TABOX51  IS  TABOX5( IDENT_INT(1)..IDENT_INT(5) );

          ARRX51  :  TABOX51 ;
          ARRX52  :  TABOX5( IDENT_INT(5)..IDENT_INT(9) );

     BEGIN

          -- INITIALIZATION OF RHS ARRAY:

          FOR  I  IN  IDENT_INT(1)..IDENT_INT(5)  LOOP
               ARRX51( I )  :=  FALSE  ; -- VALUES WILL BE:  F T F F T
          END LOOP;

          ARRX51(2) := TRUE ;

          ARRX51(5) := TRUE ;            -- RHS VALUES ARE:  F T F F T


          -- INITIALIZATION OF LHS ARRAY:

          FOR  I  IN  IDENT_INT(5)..IDENT_INT(9)  LOOP
               ARRX52( I )  :=  TRUE   ; -- VALUES WILL BE:  T F T T F
          END LOOP;

          ARRX52(6) := FALSE ;

          ARRX52(9) := FALSE ;           -- LHS VALUES ARE:  T F T T F


          -- NULL SLICE ASSIGNMENT:

          ARRX52( IDENT_INT(6)..IDENT_INT(5) ) :=
               ARRX51(
                  IDENT_INT(4)..IDENT_INT(3) ) ;


          -- CHECKING THE VALUES AFTER THE SLICE ASSIGNMENT:

          IF  ARRX52( 5 )  /=  TRUE   OR
              ARRX52( 6 )  /=  FALSE  OR
              ARRX52( 7 )  /=  TRUE   OR
              ARRX52( 8 )  /=  TRUE   OR
              ARRX52( 9 )  /=  FALSE
          THEN
               FAILED( "SLICE ASSIGNMENT NOT CORRECT (VALUES)" );
          END IF;

     EXCEPTION

          WHEN  OTHERS  =>
                FAILED( "EXCEPTION RAISED  -  SUBTEST 12" );

     END ;

     -------------------------------------------------------------------


     RESULT ;


END C52103P;
