-- C41303Q.ADA

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
-- CHECK THAT THE NOTATION  L.ALL  IS ALLOWED IF  L  IS THE NAME OF AN
--     ACCESS OBJECT DESIGNATING A RECORD, AN ARRAY, A SCALAR, OR
--     ANOTHER ACCESS OBJECT.
-- CHECK THAT IF  A  IS AN IDENTIFIER DENOTING AN ACCESS OBJECT WHICH
--     IN TURN DESIGNATES AN ACCESS OBJECT, THE FORM  A.ALL.ALL  IS
--     ACCEPTED.


-- THIS OBJECTIVE IS COVERED IN SEVERAL TESTS. IN THE FOLLOWING DIAGRAM,
--     THE PORTION COVERED BY THE CURRENT TEST IS MARKED BY 'X' .


--                              ||   ASSIGNMT  |  PROC. PARAMETERS
--                              ||  ():=  :=() | IN   OUT    IN OUT
--      ========================||=============|====================
--                 ACC REC      ||             |
--                --------------||-------------|--------------------
--       1 '.ALL'  ACC ARR      ||             |
--                --------------||-------------|--------------------
--                 ACC SCLR     ||             |
--      ========================||=============|====================
--                 ACC ACC REC  ||             |     XXXXXXXXX
--                --------------||-------------|--------------------
--       1 '.ALL'  ACC ACC ARR  ||             |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ========================||=============|====================
--                 ACC ACC REC  ||             |
--                --------------||-------------|--------------------
--       2 '.ALL'  ACC ACC ARR  ||             |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ============================================================


-- RM  1/28/82
-- SPS 12/2/82


WITH REPORT;
USE REPORT;
PROCEDURE C41303Q IS


BEGIN

     TEST ( "C41303Q" , "CHECK THAT THE NOTATION  L.ALL  IS ALLOWED IF"
                    &   "  L  IS THE NAME OF AN ACCESS OBJECT"
                    &   " DESIGNATING  ANOTHER ACCESS OBJECT" );


     -------------------------------------------------------------------
     ---------------  ACCESS TO ACCESS TO RECORD  ----------------------

     DECLARE

          TYPE  REC  IS

               RECORD
                    A , B , C : INTEGER ;
               END RECORD ;


          TYPE  ACCREC  IS  ACCESS REC ;

          ACCREC_CONST    :  ACCREC  :=  NEW REC'( 7 , 8 , 9 );
          ACCREC_VAR      :  ACCREC  :=    ACCREC_CONST ;
          ACCREC_VAR0     :  ACCREC  :=    ACCREC_CONST ;
          ACCREC_CONST2   :  ACCREC  :=  NEW REC'( 17 , 18 , 19 );

          TYPE ACC_ACCREC  IS  ACCESS ACCREC ;

          ACC_ACCREC_VAR  :  ACC_ACCREC :=  NEW ACCREC'(ACCREC_CONST2);
          ACC_ACCREC_VAR0 :  ACC_ACCREC :=  NEW ACCREC'(ACCREC_CONST2);

          PROCEDURE  R_ASSIGN( R_IN    : IN      ACCREC ;
                               R_INOUT : IN OUT  ACCREC )  IS
          BEGIN
               ACCREC_VAR   :=  R_IN    ;
               ACCREC_VAR0  :=  R_INOUT ;
          END ;


          PROCEDURE  L_ASSIGN( L_OUT   :    OUT  ACCREC ;
                               L_INOUT : IN OUT  ACCREC )  IS
          BEGIN
               L_OUT   :=  ACCREC_CONST ;
               L_INOUT :=  ACCREC_CONST ;
          END ;


     BEGIN


          R_ASSIGN( ACC_ACCREC_VAR.ALL , ACC_ACCREC_VAR0.ALL );

          IF  ACCREC_VAR /= ACCREC_CONST2
          THEN
               FAILED( "ACC. RECORD, RIGHT SIDE (1), WRONG VAL.");
          END IF;

          IF  ACCREC_VAR0 /= ACCREC_CONST2
          THEN
               FAILED( "ACC. RECORD, RIGHT SIDE (2), WRONG VAL.");
          END IF;


          L_ASSIGN( ACC_ACCREC_VAR.ALL , ACC_ACCREC_VAR0.ALL );

          IF  ACCREC_CONST /= ACC_ACCREC_VAR.ALL 
          THEN
              FAILED( "ACC. RECORD, LEFT SIDE (1), WRONG VAL." );
          END IF;

          IF  ACCREC_CONST /= ACC_ACCREC_VAR0.ALL 
          THEN
               FAILED( "ACC. RECORD, LEFT SIDE (2), WRONG VAL." );
          END IF;


     END ;

     -------------------------------------------------------------------

     RESULT;


END C41303Q;
