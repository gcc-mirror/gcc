-- C52010A.ADA

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
-- CHECK THAT RECORD ASSIGNMENTS USE "COPY" SEMANTICS. (PART I).


-- FACTORS AFFECTING THE SITUATION TO BE TESTED:
--
--        COMPONENT TYPE                     * INTEGER
--                                           * BOOLEAN (OMITTED)
--                                           * CHARACTER (OMITTED)
--                                           * USER-DEFINED ENUMERATION
--
--        DERIVED VS. NON-DERIVED
--
--        TYPE VS. SUBTYPE
--
--        ORDER OF COMPONENT ASSIGNMENTS     * LEFT-TO-RIGHT
--                                           * RIGHT-TO-LEFT
--                                           * INSIDE-OUT
--                                           * OUTSIDE IN


-- RM 02/23/80
-- SPS 3/21/83

WITH REPORT;
PROCEDURE C52010A IS

     USE REPORT;

     TYPE  ENUM  IS  ( AA , BB , CC , DD , EE , FF , GG , HH ,
                       II , JJ , KK , LL , MM , NN , PP , QQ ,
                                 TT , UU , VV , WW , XX , YY );

BEGIN

     TEST ( "C52010A" ,  "CHECK THAT RECORD ASSIGNMENTS USE ""COPY""" &
                         " SEMANTICS" );


     DECLARE
          TYPE  REC  IS
               RECORD
                    X , Y  :  INTEGER ;
               END RECORD;
          R  :  REC ;
     BEGIN

          R  :=  ( 5 , 8 ) ;
          R  :=  ( X => 1 , Y => R.X ) ;
          IF  R  /=  ( 1 , 5 )  THEN
               FAILED ( "WRONG VALUE  (1)" );
          END IF;

          R  :=  ( 5 , 8 ) ;
          R  :=  ( Y => 1 , X => R.Y ) ;
          IF  R  /=  ( 8 , 1 )  THEN
               FAILED ( "WRONG VALUE  (2)" );
          END IF;

          R  :=  ( 5 , 8 ) ;
          R  :=  ( R.Y+1 , R.X+1 ) ;
          IF  R  /=  ( 9 , 6 ) THEN
               FAILED ( "WRONG VALUE  (3)" );
          END IF;

     END;

     DECLARE
          TYPE  REC3  IS
               RECORD
                    DEEP0     :  INTEGER   ;
                    DEEP      :  INTEGER   ;
               END RECORD;
          TYPE  REC2  IS
               RECORD
                    YX        :  REC3      ;
                    MODERATE  :  INTEGER   ;
               END RECORD;
          TYPE  REC  IS
               RECORD
                    SHALLOW   :  INTEGER   ;
                    YZ        :  REC2      ;
               END RECORD;
          R  :  REC ;
     BEGIN
          R  :=  ( 0              , ((5,  1         ), 2             ));
          R  :=  ( R.YZ.MODERATE+8, ((7, R.SHALLOW+1),R.YZ.YX.DEEP+99));
          IF R/= (              10, ((7,           1),            100))
          THEN
               FAILED ( "WRONG VALUE  (4)" );
          END IF;
     END;


     DECLARE
          TYPE  SUB_ENUM  IS  NEW ENUM RANGE  AA..DD ;
          TYPE  REC  IS
               RECORD
                    X , Y  :  SUB_ENUM ;
               END RECORD;
          R  :  REC ;
     BEGIN
          R  :=  ( AA , CC ) ;
          R  :=  ( X => BB , Y => R.X ) ;
          IF  R  /=  ( BB , AA )  THEN
               FAILED ( "WRONG VALUE  (5)" );
          END IF;

          R  :=  ( AA , CC ) ;
          R  :=  ( Y => BB , X => R.Y ) ;
          IF  R  /=  ( CC , BB )  THEN
               FAILED ( "WRONG VALUE  (6)" );
          END IF;

          R  :=  ( AA , CC ) ;
          R  :=  ( SUB_ENUM'SUCC( R.Y ) , SUB_ENUM'SUCC( R.X ) ) ;
          IF  R /= ( DD , BB )  THEN
               FAILED ( "WRONG VALUE  (7)" );
          END IF;

     END;


     DECLARE
          TYPE  REC3  IS
               RECORD
                    DEEP0      :  ENUM ;
                    DEEP       :  ENUM ;
               END RECORD;     
          TYPE  REC2  IS
               RECORD
                    YX         :  REC3 ;
                    MODERATE   :  ENUM ;
               END RECORD;
          TYPE  REC  IS
               RECORD
                    SHALLOW    :  ENUM ;
                    YZ         :  REC2 ;
               END RECORD;
          R  :  REC ;
     BEGIN

          R  :=  (    TT ,
                   (( YY , II ) ,
                      AA ) ) ;

          R  :=  (    ENUM'SUCC(ENUM'SUCC( R.YZ.MODERATE )) ,
                   (( AA , ENUM'SUCC( R.SHALLOW ) ) ,
                    ( ENUM'SUCC(ENUM'SUCC(ENUM'SUCC(ENUM'SUCC(
                                R.YZ.YX.DEEP )))) ) ) ) ;

          IF R/= (    CC ,
                   (( AA , UU ) ,
                      MM ) )
          THEN
               FAILED ( "WRONG VALUE  (8)" );
          END IF;

     END;

     RESULT ;

END C52010A ;
