-- C45220C.ADA

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
--    OPERANDS OF A TYPE DERIVED FROM THE TYPE 'BOOLEAN'
--    (IN PARTICULAR, FOR OPERANDS HAVING DIFFERENT SUBTYPES).

-- THIS TEST IS DERIVED FROM  C45220A.ADA .


-- RM    27 OCTOBER 1980
-- JWC 7/8/85   RENAMED TO -AB


WITH  REPORT ;
PROCEDURE  C45220C  IS


     USE REPORT;

     TYPE  NB  IS  NEW BOOLEAN ;

     SUBTYPE  T1  IS  NB RANGE NB'(FALSE)..NB'(FALSE) ;
     SUBTYPE  T2  IS  NB RANGE NB'(TRUE )..NB'(TRUE );
     SUBTYPE  T3  IS  NB RANGE NB'(FALSE)..NB'(TRUE );
     SUBTYPE  T4  IS  T3 RANGE NB'(TRUE )..NB'(TRUE );

     FVAR1   : T1 := NB'(FALSE) ;
     TVAR1   : T2 := NB'(TRUE );
     FVAR2   : T3 := NB'(FALSE) ;
     TVAR2   : T4 := NB'(TRUE );

     ERROR_COUNT : INTEGER := 0 ;   -- INITIAL VALUE ESSENTIAL

     PROCEDURE  BUMP  IS
     BEGIN
          ERROR_COUNT := ERROR_COUNT + 1 ;
     END BUMP ;

     FUNCTION  IDENT_NEW_BOOL( THE_ARGUMENT : NB )  RETURN  NB  IS
     BEGIN
          IF  EQUAL(2,2)  THEN  RETURN THE_ARGUMENT;
          ELSE  RETURN  NB'(FALSE) ;
          END IF;
     END ;


BEGIN


     TEST( "C45220C" , "CHECK THAT  '='  AND  '/='  PRODUCE CORRECT" &
                       " RESULTS ON DERIVED-BOOLEAN-TYPE OPERANDS" ) ;

     -- 32  CASES ( 2 * 2  ORDERED PAIRS OF OPERAND VALUES,
     --               2    OPERATORS : '=' , '/=' ,
     --               4    VARIABLE/LITERAL FOR LEFT OPERAND,
     --                    VARIABLE/LITERAL FOR RIGHT OPERAND.


     --  'BUMP'  MEANS  'BUMP THE ERROR COUNT'

     FVAR1   := IDENT_NEW_BOOL( NB'(FALSE) ) ;
     TVAR1   := IDENT_NEW_BOOL( NB'(TRUE )) ;
     FVAR2   := IDENT_NEW_BOOL( NB'(FALSE) ) ;
     TVAR2   := IDENT_NEW_BOOL( NB'(TRUE )) ;

     IF  NB'(FALSE) =  NB'(FALSE)  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      FVAR1  =  NB'(FALSE)  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  NB'(FALSE) =      FVAR2   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      FVAR2  =      FVAR1   THEN  NULL ;  ELSE  BUMP ;  END IF;

     IF  NB'(FALSE) =  NB'(TRUE )  THEN  BUMP ;                END IF;
     IF      FVAR1  =  NB'(TRUE )  THEN  BUMP ;                END IF;
     IF  NB'(FALSE) =      TVAR2   THEN  BUMP ;                END IF;
     IF      FVAR2  =      TVAR1   THEN  BUMP ;                END IF;

     IF  NB'(TRUE ) =  NB'(FALSE)  THEN  BUMP ;                END IF;
     IF  NB'(TRUE ) =      FVAR1   THEN  BUMP ;                END IF;
     IF      TVAR2  =  NB'(FALSE)  THEN  BUMP ;                END IF;
     IF      TVAR1  =      FVAR2   THEN  BUMP ;                END IF;

     IF  NB'(TRUE ) =  NB'(TRUE )  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      TVAR1  =  NB'(TRUE )  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  NB'(TRUE ) =      TVAR2   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      TVAR2  =      TVAR1   THEN  NULL ;  ELSE  BUMP ;  END IF;


     IF  NB'(FALSE) /= NB'(FALSE)  THEN  BUMP ;                END IF;
     IF      FVAR1  /= NB'(FALSE)  THEN  BUMP ;                END IF;
     IF  NB'(FALSE) /=     FVAR2   THEN  BUMP ;                END IF;
     IF      FVAR2  /=     FVAR1   THEN  BUMP ;                END IF;

     IF  NB'(FALSE) /= NB'(TRUE )  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      FVAR1  /= NB'(TRUE )  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  NB'(FALSE) /=     TVAR2   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      FVAR2  /=     TVAR1   THEN  NULL ;  ELSE  BUMP ;  END IF;

     IF  NB'(TRUE ) /= NB'(FALSE)  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  NB'(TRUE ) /=     FVAR1   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      TVAR2  /= NB'(FALSE)  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF      TVAR1  /=     FVAR2   THEN  NULL ;  ELSE  BUMP ;  END IF;

     IF  NB'(TRUE ) /= NB'(TRUE )  THEN  BUMP ;                END IF;
     IF      TVAR1  /= NB'(TRUE )  THEN  BUMP ;                END IF;
     IF  NB'(TRUE ) /=     TVAR2   THEN  BUMP ;                END IF;
     IF      TVAR2  /=     TVAR1   THEN  BUMP ;                END IF;


     IF  ERROR_COUNT /=0  THEN
          FAILED( "(IN)EQUALITY OF N_BOOLEAN VALUES - FAILURE1" );
     END IF;


     RESULT ;


END C45220C;
