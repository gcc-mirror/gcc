-- C74209A.ADA

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
-- CHECK THAT OUTSIDE A PACKAGE WHICH DEFINES PRIVATE TYPES AND LIMITED
--    PRIVATE TYPES  IT IS POSSIBLE TO DECLARE SUBPROGRAMS WHICH USE
--    THOSE TYPES AS TYPES FOR PARAMETERS (OF ANY MODE EXCEPT OUT FOR A
--    LIMITED TYPE) OR AS THE TYPE FOR THE RESULT (FOR FUNCTION
--    SUBPROGRAMS). 

-- RM 07/14/81


WITH REPORT;
PROCEDURE  C74209A  IS

     USE  REPORT;

BEGIN

     TEST( "C74209A" , "CHECK THAT PROCEDURE SIGNATURES CAN USE " &
                       "PRIVATE TYPES" );

     DECLARE

          PACKAGE  PACK  IS

               TYPE  LIM_PRIV  IS LIMITED PRIVATE;
               TYPE  PRIV      IS         PRIVATE;
               PRIV_CONST_IN   :  CONSTANT  PRIV;
               PRIV_CONST_OUT  :  CONSTANT  PRIV;
               FUNCTION  PACKAGED( X: IN INTEGER )  RETURN LIM_PRIV;
               FUNCTION  EQUALS( X , Y : LIM_PRIV ) RETURN BOOLEAN ;
               PROCEDURE ASSIGN( X : IN  LIM_PRIV; Y : OUT LIM_PRIV );

          PRIVATE

               TYPE  LIM_PRIV  IS NEW INTEGER;
               TYPE  PRIV      IS NEW STRING( 1..5 );
               PRIV_CONST_IN   :  CONSTANT  PRIV :=  "ABCDE";
               PRIV_CONST_OUT  :  CONSTANT  PRIV :=  "FGHIJ";

          END  PACK;


          PRIV_VAR_1 ,     PRIV_VAR_2      :  PACK.PRIV;
          LIM_PRIV_VAR_1 , LIM_PRIV_VAR_2  :  PACK.LIM_PRIV;


          USE  PACK;


          PACKAGE BODY  PACK  IS

               FUNCTION  PACKAGED( X: IN INTEGER )  RETURN LIM_PRIV  IS
               BEGIN
                    RETURN  LIM_PRIV(X);
               END  PACKAGED;

               FUNCTION  EQUALS( X , Y : LIM_PRIV ) RETURN BOOLEAN  IS
               BEGIN
                    RETURN  X = Y ;
               END  EQUALS;

               PROCEDURE ASSIGN( X : IN  LIM_PRIV; Y : OUT LIM_PRIV) IS
               BEGIN
                    Y := X;
               END  ASSIGN;

          END  PACK;


          PROCEDURE  PROC1( X : IN OUT PACK.PRIV;
                            Y : IN     PACK.PRIV := PACK.PRIV_CONST_IN;
                            Z : OUT    PACK.PRIV;
                            U :        PACK.PRIV )  IS
          BEGIN

               IF  X /= PACK.PRIV_CONST_IN  OR
                   Y /= PACK.PRIV_CONST_IN  OR
                   U /= PACK.PRIV_CONST_IN
               THEN
                    FAILED( "WRONG INPUT VALUES  -  PROC1" );
               END IF;

               X := PACK.PRIV_CONST_OUT;
               Z := PACK.PRIV_CONST_OUT;

          END  PROC1;


          PROCEDURE  PROC2( X : IN OUT LIM_PRIV;
                            Y : IN     LIM_PRIV;
                            Z : IN OUT LIM_PRIV;
                            U :        LIM_PRIV )  IS
          BEGIN

               IF  NOT(EQUALS( X , PACKAGED(17) ))  OR
                   NOT(EQUALS( Y , PACKAGED(17) ))  OR
                   NOT(EQUALS( U , PACKAGED(17) ))
               THEN
                    FAILED( "WRONG INPUT VALUES  -  PROC2" );
               END IF;

               ASSIGN( PACKAGED(13) , X );
               ASSIGN( PACKAGED(13) , Z );

          END  PROC2;


          FUNCTION  FUNC1( Y : IN  PRIV  :=  PRIV_CONST_IN;
                           U :     PRIV  )   RETURN  PRIV  IS
          BEGIN

               IF  Y /= PRIV_CONST_IN  OR
                   U /= PRIV_CONST_IN
               THEN
                    FAILED( "WRONG INPUT VALUES  -  FUNC1" );
               END IF;

               RETURN  PRIV_CONST_OUT;

          END  FUNC1;


          FUNCTION  FUNC2( Y : IN  LIM_PRIV;
                           U :     LIM_PRIV )  RETURN  LIM_PRIV  IS
          BEGIN

               IF  NOT(EQUALS( Y , PACKAGED(17) ))  OR
                   NOT(EQUALS( U , PACKAGED(17) ))
               THEN
                    FAILED( "WRONG INPUT VALUES  -  FUNC2" );
               END IF;

               RETURN  PACKAGED(13);

          END  FUNC2;


     BEGIN

          --------------------------------------------------------------

          PRIV_VAR_1 := PRIV_CONST_IN;
          PRIV_VAR_2 := PRIV_CONST_IN;

          PROC1( PRIV_VAR_1 , Z => PRIV_VAR_2 , U => PRIV_CONST_IN );

          IF  PRIV_VAR_1 /= PACK.PRIV_CONST_OUT  OR
              PRIV_VAR_2 /= PACK.PRIV_CONST_OUT
          THEN
               FAILED( "WRONG OUTPUT VALUES  -  PROC1" );
          END IF;

          --------------------------------------------------------------

          ASSIGN( PACKAGED(17) , LIM_PRIV_VAR_1 );
          ASSIGN( PACKAGED(17) , LIM_PRIV_VAR_2 );

          PROC2( LIM_PRIV_VAR_1 , PACKAGED(17) ,
                 LIM_PRIV_VAR_2 , PACKAGED(17) );

          IF  NOT(EQUALS( LIM_PRIV_VAR_1 , PACKAGED(13) ))  OR
              NOT(EQUALS( LIM_PRIV_VAR_2 , PACKAGED(13) ))
          THEN
               FAILED( "WRONG OUTPUT VALUES  -  PROC2" );
          END IF;

          --------------------------------------------------------------

          PRIV_VAR_1 := PRIV_CONST_IN;
          PRIV_VAR_2 := PRIV_CONST_IN;

          PRIV_VAR_1 :=
              FUNC1( PRIV_VAR_1 , U => PRIV_CONST_IN );

          IF  PRIV_VAR_1 /= PACK.PRIV_CONST_OUT  
          THEN
               FAILED( "WRONG OUTPUT VALUES  -  FUNC1" );
          END IF;

          --------------------------------------------------------------

          ASSIGN( PACKAGED(17) , LIM_PRIV_VAR_1 );
          ASSIGN( PACKAGED(17) , LIM_PRIV_VAR_2 );

          ASSIGN( FUNC2( LIM_PRIV_VAR_1 , PACKAGED(17)) ,
                  LIM_PRIV_VAR_1 );

          IF  NOT(EQUALS( LIM_PRIV_VAR_1 , PACKAGED(13) ))
          THEN
               FAILED( "WRONG OUTPUT VALUES  -  FUNC2" );
          END IF;

          --------------------------------------------------------------

     END;


     RESULT;


END C74209A;
