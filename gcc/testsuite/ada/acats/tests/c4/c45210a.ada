-- C45210A.ADA

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
-- CHECK THAT AN ENUMERATION IMPOSING AN "UNNATURAL" ORDER ON ALPHABETIC
--    CHARACTERS CORRECTLY EVALUATES THE ORDERING OPERATORS.


-- RM    15 OCTOBER 1980
-- JWC 7/8/85   RENAMED TO -AB


WITH  REPORT ;
PROCEDURE  C45210A  IS

     USE REPORT;

     TYPE  T  IS  ( 'S' , 'P' , 'M' , 'R' );

     MVAR  : T := T'('M') ;
     PVAR  : T := T'('P') ;
     RVAR  : T := T'('R') ;
     SVAR  : T := T'('S') ;

     ERROR_COUNT : INTEGER := 0 ;   -- INITIAL VALUE ESSENTIAL

     PROCEDURE  BUMP  IS
     BEGIN
          ERROR_COUNT := ERROR_COUNT +1 ;
     END BUMP ;


BEGIN

     TEST( "C45210A" , "CHECK THAT AN ENUMERATION IMPOSING" &
                       " AN ""UNNATURAL"" ORDER ON ALPHABETIC" &
                       " CHARACTERS  CORRECTLY EVALUATES THE " &
                       " ORDERING OPERATORS" ) ;

     -- 256 CASES ( 4 * 4  ORDERED PAIRS OF OPERAND VALUES,
     --               4    ORDERING OPERATORS: '<' , '<=' , '>' , '>='
     --                         (IN THE TABLE:  A  ,  B   ,  C  ,  D   )
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
     --    ( VV , ALPHA ) FOR ALL 4 OPERATORS.

    -----------------------------------------------------------------

     -- PART 1

     --  'BUMP'  MEANS  'BUMP THE ERROR COUNT'

     IF  T'(SVAR) <  T'(SVAR)  THEN  BUMP ;               END IF;
     IF  T'(SVAR) <= T'('P' )  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'('S' ) >  T'(MVAR)  THEN  BUMP ;               END IF;
     IF  T'('S' ) >= T'('R' )  THEN  BUMP ;               END IF;

     IF  T'('P' ) >  T'('S' )  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'('P' ) >= T'(PVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(PVAR) <  T'('M' )  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(PVAR) <= T'(RVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;

     IF  T'(MVAR) >= T'('S' )  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(MVAR) >  T'(PVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'('M' ) <= T'('M' )  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'('M' ) <  T'(RVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;

     IF  T'('R' ) <= T'(SVAR)  THEN  BUMP ;               END IF;
     IF  T'('R' ) <  T'('P' )  THEN  BUMP ;               END IF;
     IF  T'(RVAR) >= T'(MVAR)  THEN  NULL;  ELSE  BUMP ;  END IF;
     IF  T'(RVAR) >  T'('R' )  THEN  BUMP ;               END IF;


     IF  ERROR_COUNT /= 0  THEN
          FAILED( """UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE1" );
     END IF;

    -----------------------------------------------------------------

     -- PART 2

     -- 'BUMP'  MEANS  'INCREASE THE COUNT FOR THE NUMBER OF <TRUE>S'

     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 4 VALUES
          FOR  BVAR  IN  T'FIRST..T'('P')  LOOP    -- 2 VALUES

               IF  AVAR <  BVAR  THEN  BUMP ;  END IF;   -- COUNT +:=  1

          END LOOP;
     END LOOP;

     IF  ERROR_COUNT /= 1  THEN   -- THIS IS A PLAIN COUNT, NOT AN
                                   --    ERROR COUNT
          FAILED( """UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE2" );
     END IF;


     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 4 VALUES
          FOR  BVAR  IN  T'FIRST..T'('P')  LOOP    -- 2 VALUES

               IF  AVAR <= BVAR  THEN  BUMP ;  END IF;   -- COUNT +:=  3

          END LOOP;
     END LOOP;

     IF  ERROR_COUNT /= 3  THEN   -- THIS IS A PLAIN COUNT, NOT AN
                                   --    ERROR COUNT
          FAILED( """UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE3" );
     END IF;


     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 4 VALUES
          FOR  BVAR  IN  T'FIRST..T'('P')  LOOP    -- 2 VALUES

               IF  AVAR >  BVAR  THEN  BUMP ;  END IF;   -- COUNT +:=  5

          END LOOP;
     END LOOP;

     IF  ERROR_COUNT /= 5  THEN   -- THIS IS A PLAIN COUNT, NOT AN
                                   --    ERROR COUNT
          FAILED( """UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE4" );
     END IF;


     ERROR_COUNT := 0 ;

     FOR  AVAR IN  T'FIRST..T'LAST  LOOP           -- 4 VALUES
          FOR  BVAR  IN  T'FIRST..T'('P')  LOOP    -- 2 VALUES

               IF  AVAR >= BVAR  THEN  BUMP ;  END IF;   -- COUNT +:=  7

          END LOOP;
     END LOOP;

     IF  ERROR_COUNT /= 7  THEN   -- THIS IS A PLAIN COUNT, NOT AN
                                   --    ERROR COUNT
          FAILED( """UNNATURAL"" ORDER ON CHARACTER TYPES - FAILURE5" );
     END IF;


     RESULT;

END C45210A;
