-- C45662A.ADA

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
-- CHECK THE TRUTH TABLE FOR  'NOT' .

-- THE COMBINATIONS OF  'NOT'  WITH  'AND' , 'OR' , 'XOR'  ARE TESTED
--    IN C45101(A,G).


-- RM    28 OCTOBER 1980
-- TBN 10/21/85     RENAMED FROM C45401A.ADA.


WITH  REPORT ;
PROCEDURE  C45662A  IS

     USE REPORT;

     TVAR , FVAR , CVAR : BOOLEAN := FALSE ; -- INITIAL VALUE IRRELEVANT
     ERROR_COUNT : INTEGER := 0 ;            -- INITIAL VALUE ESSENTIAL

     PROCEDURE  BUMP  IS
     BEGIN
          ERROR_COUNT  :=  ERROR_COUNT + 1 ;
     END BUMP ;

BEGIN

     TEST( "C45662A" , "CHECK THE TRUTH TABLE FOR  'NOT'" ) ;

     FOR  A  IN  BOOLEAN  LOOP

          CVAR  :=  NOT A ;

          IF  NOT A  THEN
               IF  A  THEN  BUMP ;
               END IF ;
          END IF;

          IF  CVAR  THEN
               IF  A  THEN  BUMP ;
               END IF ;
          END IF;

          IF  NOT( NOT( NOT( NOT(   CVAR   ))))
          THEN
               IF  A  THEN  BUMP ;
               END IF ;
          END IF;

     END LOOP ;

     FOR  I  IN  1..2  LOOP

          CVAR  :=  NOT ( I > 1 ) ;

          IF  NOT ( I > 1 )  THEN
               IF  I>1  THEN  BUMP ;
               END IF ;
          END IF;

          IF  CVAR  THEN
               IF  I>1  THEN  BUMP ;
               END IF ;
          END IF;

     END LOOP ;

     IF  NOT TRUE   THEN  BUMP ;                END IF ;
     IF  NOT FALSE  THEN  NULL ;  ELSE  BUMP ;  END IF ;

     TVAR := IDENT_BOOL( TRUE );
     FVAR := IDENT_BOOL( FALSE );

     IF  NOT TVAR  THEN  BUMP ;                END IF ;
     IF  NOT FVAR  THEN  NULL ;  ELSE  BUMP ;  END IF ;


     IF  ERROR_COUNT  /= 0  THEN  FAILED( "'NOT' TRUTH TABLE" );
     END IF ;

     RESULT;

END C45662A;
