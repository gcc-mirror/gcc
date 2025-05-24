-- C55B15A.ADA

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
-- CHECK THAT IF A DISCRETE_RANGE OF THE FORM  'ST RANGE L..R'
--    RAISES AN EXCEPTION BECAUSE  L  OR  R  IS A NON-STATIC
--    EXPRESSION WHOSE VALUE IS OUTSIDE  THE RANGE OF VALUES
--    ASSOCIATED WITH  ST  (OR BECAUSE  ST'FIRST  IS NON-STATIC
--    AND  L  IS STATIC AND LESS THAN  ST'FIRST ; SIMILARLY FOR
--     ST'LAST  AND  R ), CONTROL DOES NOT ENTER THE LOOP.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- RM  04/13/81
-- SPS 11/01/82
-- BHS 07/13/84
-- EG  10/28/85  FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY
-- GJD 11/15/95  REMOVED CASE OF POTENTIALLY STATICALLY INCOMPATIBLE RANGE.

WITH SYSTEM;
WITH REPORT;
PROCEDURE  C55B15A  IS

     USE  REPORT ;

BEGIN

     TEST( "C55B15A" , "WHEN  'FOR  I  IN  ST RANGE L..R  LOOP' "     &
                       "RAISES AN EXCEPTION, CONTROL DOES NOT ENTER " &
                       "THE BODY OF THE LOOP" );

     -------------------------------------------------------------------
     ----------------- STATIC (SUB)TYPE, DYNAMIC RANGE -----------------

     DECLARE

          SUBTYPE  ST  IS  INTEGER RANGE 1..4 ;

          FIRST   :  CONSTANT INTEGER := IDENT_INT( 1) ;
          SECOND  :  CONSTANT INTEGER := IDENT_INT( 2) ;
          THIRD   :  CONSTANT INTEGER := IDENT_INT( 3) ;
          FOURTH  :  CONSTANT INTEGER := IDENT_INT( 4) ;
          FIFTH   :  CONSTANT INTEGER := IDENT_INT( 5) ;
          TENTH   :  CONSTANT INTEGER := IDENT_INT(10) ;
          ZEROTH  :  CONSTANT INTEGER := IDENT_INT( 0) ;

     BEGIN

          BEGIN

               FOR  I  IN  ST RANGE 3..TENTH  LOOP
                    FAILED( "EXCEPTION NOT RAISED (I1)" );
               END LOOP;

          EXCEPTION

               WHEN  CONSTRAINT_ERROR => NULL ;
               WHEN  OTHERS           =>
                    FAILED( "WRONG EXCEPTION RAISED (I1)" );

          END ;


          BEGIN

               FOR  I  IN  ST RANGE 0..THIRD  LOOP
                    FAILED( "EXCEPTION NOT RAISED (I2)" );
               END LOOP;

          EXCEPTION

               WHEN  CONSTRAINT_ERROR => NULL ;
               WHEN  OTHERS           =>
                    FAILED( "WRONG EXCEPTION RAISED (I2)" );

          END ;
     END ;


     -------------------------------------------------------------------
     ----------------- DYNAMIC (SUB)TYPE, STATIC RANGE -----------------

     DECLARE

          TYPE  ENUM   IS  ( AMINUS , A,B,C,D,E,  F,G,H,I,J );

          SUBTYPE  ST  IS  ENUM RANGE ENUM'VAL( IDENT_INT( 1) ) ..
                                      ENUM'VAL( IDENT_INT( 4) ) ;

          FIRST   :  CONSTANT ENUM := A ;
          SECOND  :  CONSTANT ENUM := B ;
          THIRD   :  CONSTANT ENUM := C ;
          FOURTH  :  CONSTANT ENUM := D ;
          FIFTH   :  CONSTANT ENUM := E ;
          TENTH   :  CONSTANT ENUM := J ;
          ZEROTH  :  CONSTANT ENUM := AMINUS ;

     BEGIN

          BEGIN

               FOR  I  IN  ST RANGE C..TENTH  LOOP
                    FAILED( "EXCEPTION NOT RAISED (E1)" );
               END LOOP;

          EXCEPTION

               WHEN  CONSTRAINT_ERROR => NULL ;
               WHEN  OTHERS           =>
                    FAILED( "WRONG EXCEPTION RAISED (E1)" );

          END ;


          BEGIN

               FOR  I  IN  ST RANGE AMINUS..THIRD  LOOP
                    FAILED( "EXCEPTION NOT RAISED (E2)" );
               END LOOP;

          EXCEPTION

               WHEN  CONSTRAINT_ERROR => NULL ;
               WHEN  OTHERS           =>
                    FAILED( "WRONG EXCEPTION RAISED (E2)" );

          END ;

     END ;


     DECLARE

          SUBTYPE  ST  IS  CHARACTER RANGE IDENT_CHAR( 'A' ) ..
                                           IDENT_CHAR( 'D' ) ;

          FIRST   :  CONSTANT CHARACTER := 'A' ;
          SECOND  :  CONSTANT CHARACTER := 'B' ;
          THIRD   :  CONSTANT CHARACTER := 'C' ;
          FOURTH  :  CONSTANT CHARACTER := 'D' ;
          FIFTH   :  CONSTANT CHARACTER := 'E' ;
          TENTH   :  CONSTANT CHARACTER := 'J' ;
          ZEROTH  :  CONSTANT CHARACTER := '0' ;--ZERO; PRECEDES LETTERS

     BEGIN

          BEGIN

               FOR  I  IN  ST RANGE 'C'..TENTH  LOOP
                    FAILED( "EXCEPTION NOT RAISED (C1)" );
               END LOOP;

          EXCEPTION

               WHEN  CONSTRAINT_ERROR => NULL ;
               WHEN  OTHERS           =>
                    FAILED( "WRONG EXCEPTION RAISED (C1)" );

          END ;


          BEGIN

               FOR  I  IN  ST RANGE '0'..THIRD  LOOP -- ZERO..'C'
                    FAILED( "EXCEPTION NOT RAISED (C2)" );
               END LOOP;

          EXCEPTION

               WHEN  CONSTRAINT_ERROR => NULL ;
               WHEN  OTHERS           =>
                    FAILED( "WRONG EXCEPTION RAISED (C2)" );

          END ;

     END ;


     RESULT ;


END  C55B15A ;
