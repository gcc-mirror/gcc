-- C45201A.ADA

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
-- CHECK THAT  '='  AND  '/='  PRODUCE CORRECT RESULTS ON
--    ENUMERATION-TYPE OPERANDS (IN PARTICULAR, FOR OPERANDS HAVING
--    DIFFERENT SUBTYPES).

-- THIS TEST'S FRAMEWORK IS FROM  C45201B.ADA , C45210A.ADA .


-- RM    20 OCTOBER 1980
-- JWC 7/8/85   RENAMED TO -AB


WITH  REPORT ;
PROCEDURE  C45201A  IS

     USE REPORT;

     TYPE  T  IS  ( A , SLIT , B , PLIT , C , NUL , D , 'R' , E );

     --                 S-LIT ,    P-LIT ,    NUL ,     'R'   CORRESPOND
     --            TO    'S'  ,     'P'  ,    'M'  ,    'R'  IN C45210A.

     SUBTYPE  T1  IS  T RANGE A..B ;
     SUBTYPE  T2  IS  T RANGE A..C ;    -- INCLUDES  T1
     SUBTYPE  T3  IS  T RANGE B..D ;    -- INTERSECTS  T2 , T4
     SUBTYPE  T4  IS  T RANGE C..E ;    -- DISJOINT FROM  T1 , T2

     MVAR  : T3 := T'(NUL ) ;
     PVAR  : T2 := T'(PLIT) ;
     RVAR  : T4 := T'('R' ) ;
     SVAR  : T1 := T'(SLIT) ;

     ERROR_COUNT : INTEGER := 0 ;   -- INITIAL VALUE ESSENTIAL

     PROCEDURE  BUMP  IS
     BEGIN
          ERROR_COUNT := ERROR_COUNT + 1 ;
     END BUMP ;

     FUNCTION  ITSELF( THE_ARGUMENT : T )  RETURN  T  IS
     BEGIN
          IF  EQUAL(2,2)  THEN  RETURN THE_ARGUMENT;
          ELSE  RETURN  A ;
          END IF;
     END ;


BEGIN

     TEST( "C45201A" , "CHECK THAT  '='  AND  '/='  PRODUCE CORRECT" &
                       " RESULTS ON ENUMERATION-TYPE LITERALS" ) ;

     -- 128 CASES ( 4 * 4  ORDERED PAIRS OF OPERAND VALUES,
     --             2 (4)  OPERATORS (2, TWICE): '=' , '/=' , '=' , '/='
     --                          (IN THE TABLE:   A  ,  B   ,  C  ,  D )
     --                          (C45201B.ADA HAD  < <= > >= ; REVERSED)
     --               4    VARIABLE/LITERAL FOR LEFT OPERAND,
     --                    VARIABLE/LITERAL FOR RIGHT OPERAND,
     --                         (IN THE TABLE:  VV = ALPHA ,
     --                                         VL = BETA  ,
     --                                         LV = GAMMA ,
     --                                         LL = DELTA  ) RANDOMIZED
     --    INTO 16 (ONE FOR EACH PAIR OF VALUES) ACCORDING TO THE FOL-
     --    LOWING GRAECO-LATIN SQUARE (WITH ADDITIONAL PROPERTIES):

     --               RIGHT OPERAND:    'S'      'P'      'M'      'R'
     --         LEFT
     --       OPERAND:

     --         'S'                   A-ALPHA  B-BETA   C-GAMMA  D-DELTA
     --         'P'                   C-DELTA  D-GAMMA  A-BETA   B-ALPHA
     --         'M'                   D-BETA   C-ALPHA  B-DELTA  A-GAMMA
     --         'R'                   B-GAMMA  A-DELTA  D-ALPHA  C-BETA

     --    (BOTH THE LATIN DIAGONAL AND THE GREEK DIAGONAL CONTAIN 4
     --    DISTINCT LETTERS, NON-TRIVIALLY PERMUTED.)

     -- THE ABOVE DESCRIBES  PART 1  OF THE TEST.  PART 2  PERFORMS AN
     --    EXHAUSTIVE VERIFICATION OF THE 'VARIABLE VS. VARIABLE' CASE
     --    ( VV , ALPHA ) FOR BOTH OPERATORS.

    -----------------------------------------------------------------

     -- PART 1

     --  'BUMP'  MEANS  'BUMP THE ERROR COUNT'

     IF  T'(SVAR) =  T'(SVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(SVAR) /= T'(PLIT)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(SLIT) =  T'(MVAR)  THEN  BUMP ;               END IF;
     IF  T'(SLIT) /= T'('R' )  THEN  NULL;  ELSE  BUMP ;  END IF;

     IF  T'(PLIT) =  T'(SLIT)  THEN  BUMP ;               END IF;
     IF  T'(PLIT) /= T'(PVAR)  THEN  BUMP ;               END IF;
     IF  T'(PVAR) =  T'(NUL )  THEN  BUMP ;               END IF;
     IF  T'(PVAR) /= T'(RVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;

     IF  T'(MVAR) /= T'(SLIT)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(MVAR) =  T'(PVAR)  THEN  BUMP ;               END IF;
     IF  T'(NUL ) /= T'(NUL )  THEN  BUMP ;               END IF;
     IF  T'(NUL ) =  T'(RVAR)  THEN  BUMP ;               END IF;

     IF  T'('R' ) /= T'(SVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'('R' ) =  T'(PLIT)  THEN  BUMP ;               END IF;
     IF  T'(RVAR) /= T'(MVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(RVAR) =  T'('R' )  THEN  NULL;  ELSE  BUMP ;  END IF;


     IF  ERROR_COUNT /= 0  THEN
          FAILED( "EQUALITY OF ENUMERATION VALUES - FAILURE1" );
     END IF;

    -----------------------------------------------------------------

     -- PART 2

     --  'BUMP'  STILL MEANS  'BUMP THE ERROR COUNT'

     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 9 VALUES
          FOR  BVAR  IN  T'FIRST..T'LAST  LOOP     -- 9 VALUES

               IF  AVAR  = BVAR  THEN
                    IF  AVAR /= BVAR  THEN  BUMP ;  END IF;
               END IF;

               IF  AVAR /= BVAR  THEN
                    IF  AVAR  = BVAR  THEN  BUMP ;  END IF;
               END IF;

          END LOOP;
     END LOOP;

     IF  ERROR_COUNT /= 0  THEN
          FAILED( "EQUALITY OF ENUMERATION VALUES - FAILURE2" );
     END IF;


     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 9 VALUES

          FOR  BVAR  IN  T'FIRST..T'LAST   LOOP    -- 9 VALUES

               IF ( AVAR /= BVAR ) /= ( T'POS(AVAR) /= T'POS(BVAR) )THEN
                    BUMP ;
               END IF;

               IF ( AVAR  = BVAR ) /= ( T'POS(AVAR)  = T'POS(BVAR) )THEN
                    BUMP ;
               END IF;

          END LOOP;

     END LOOP;

     IF  ERROR_COUNT /= 0  THEN
          FAILED( "EQUALITY OF ENUMERATION VALUES - FAILURE3" );
     END IF;

     ERROR_COUNT := 0 ;

     FOR  IVAR IN  0..8  LOOP                      -- 9 VALUES

          FOR  JVAR  IN  0..8   LOOP               -- 9 VALUES

               IF ( IVAR /= JVAR ) /= ( T'VAL(IVAR) /= T'VAL(JVAR) )THEN
                    BUMP ;
               END IF;

               IF ( IVAR  = JVAR ) /= ( T'VAL(IVAR)  = T'VAL(JVAR) )THEN
                    BUMP ;
               END IF;

          END LOOP;

     END LOOP;

     IF  ERROR_COUNT /= 0  THEN
          FAILED( "EQUALITY OF ENUMERATION VALUES - FAILURE4" );
     END IF;


     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP    -- 9 VALUES (THE DIAGONAL)

          IF  AVAR  = ITSELF(AVAR)  THEN NULL;  ELSE BUMP;  END IF;
          IF  AVAR /= ITSELF(AVAR)  THEN BUMP;              END IF;

     END LOOP;

     IF  ERROR_COUNT /= 0  THEN
          FAILED( "EQUALITY OF ENUMERATION VALUES - FAILURE5" );
     END IF;


     -- 'BUMP'  MEANS  'INCREASE THE COUNT FOR THE NUMBER OF <TRUE>S'

     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 9 VALUES
          FOR  BVAR  IN  T'FIRST..T'LAST  LOOP     -- 9 VALUES

               IF  AVAR /= BVAR  THEN  BUMP ;  END IF;   -- COUNT +:= 72

          END LOOP;
     END LOOP;

     IF  ERROR_COUNT /= 72  THEN   -- THIS IS A PLAIN COUNT, NOT AN
                                   --    ERROR COUNT
          FAILED( "EQUALITY OF ENUMERATION VALUES - FAILURE6" );
     END IF;


     RESULT;

END C45201A;
