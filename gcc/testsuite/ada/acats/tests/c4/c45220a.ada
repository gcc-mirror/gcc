-- C45220A.ADA

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
--    BOOLEAN-TYPE OPERANDS (IN PARTICULAR, FOR OPERANDS HAVING
--    DIFFERENT SUBTYPES).

-- THIS TEST IS DERIVED FROM  C45201A.ADA .


-- RM    27 OCTOBER 1980
-- JWC 7/8/85   RENAMED TO -AB


WITH  REPORT ;
PROCEDURE  C45220A  IS


     USE REPORT;

     SUBTYPE  T1  IS  BOOLEAN RANGE FALSE..FALSE ;
     SUBTYPE  T2  IS  BOOLEAN RANGE TRUE..TRUE ;
     SUBTYPE  T3  IS  BOOLEAN RANGE FALSE..TRUE ;
     SUBTYPE  T4  IS  T3 RANGE TRUE..TRUE ;

     FVAR1  : T1 := FALSE ;
     TVAR1  : T2 := TRUE ;
     FVAR2  : T3 := FALSE ;
     TVAR2  : T4 := TRUE ;

     ERROR_COUNT : INTEGER := 0 ;   -- INITIAL VALUE ESSENTIAL

     PROCEDURE  BUMP  IS
     BEGIN
          ERROR_COUNT := ERROR_COUNT + 1 ;
     END BUMP ;


BEGIN


     TEST( "C45220A" , "CHECK THAT  '='  AND  '/='  PRODUCE CORRECT" &
                       " RESULTS ON BOOLEAN-TYPE OPERANDS" ) ;

     -- 32  CASES ( 2 * 2  ORDERED PAIRS OF OPERAND VALUES,
     --               2    OPERATORS : '=' , '/=' ,
     --               4    VARIABLE/LITERAL FOR LEFT OPERAND,
     --                    VARIABLE/LITERAL FOR RIGHT OPERAND.


     --  'BUMP'  MEANS  'BUMP THE ERROR COUNT'

     FVAR1  := IDENT_BOOL( FALSE ) ;
     TVAR1  := IDENT_BOOL( TRUE ) ;
     FVAR2  := IDENT_BOOL( FALSE ) ;
     TVAR2  := IDENT_BOOL( TRUE ) ;

     IF  FALSE =  FALSE  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  FVAR1 =  FALSE  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  FALSE =  FVAR2  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  FVAR2 =  FVAR1  THEN  NULL ;  ELSE  BUMP ;  END IF;

     IF  FALSE =  TRUE   THEN  BUMP ;                END IF;
     IF  FVAR1 =  TRUE   THEN  BUMP ;                END IF;
     IF  FALSE =  TVAR2  THEN  BUMP ;                END IF;
     IF  FVAR2 =  TVAR1  THEN  BUMP ;                END IF;

     IF  TRUE  =  FALSE  THEN  BUMP ;                END IF;
     IF  TRUE  =  FVAR1  THEN  BUMP ;                END IF;
     IF  TVAR2 =  FALSE  THEN  BUMP ;                END IF;
     IF  TVAR1 =  FVAR2  THEN  BUMP ;                END IF;

     IF  TRUE  =  TRUE   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  TVAR1 =  TRUE   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  TRUE  =  TVAR2  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  TVAR2 =  TVAR1  THEN  NULL ;  ELSE  BUMP ;  END IF;


     IF  FALSE /= FALSE  THEN  BUMP ;                END IF;
     IF  FVAR1 /= FALSE  THEN  BUMP ;                END IF;
     IF  FALSE /= FVAR2  THEN  BUMP ;                END IF;
     IF  FVAR2 /= FVAR1  THEN  BUMP ;                END IF;

     IF  FALSE /= TRUE   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  FVAR1 /= TRUE   THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  FALSE /= TVAR2  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  FVAR2 /= TVAR1  THEN  NULL ;  ELSE  BUMP ;  END IF;

     IF  TRUE  /= FALSE  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  TRUE  /= FVAR1  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  TVAR2 /= FALSE  THEN  NULL ;  ELSE  BUMP ;  END IF;
     IF  TVAR1 /= FVAR2  THEN  NULL ;  ELSE  BUMP ;  END IF;

     IF  TRUE  /= TRUE   THEN  BUMP ;                END IF;
     IF  TVAR1 /= TRUE   THEN  BUMP ;                END IF;
     IF  TRUE  /= TVAR2  THEN  BUMP ;                END IF;
     IF  TVAR2 /= TVAR1  THEN  BUMP ;                END IF;


     IF  ERROR_COUNT /=0  THEN
          FAILED( "(IN)EQUALITY OF BOOLEAN VALUES - FAILURE1" );
     END IF;


     RESULT ;


END C45220A;
