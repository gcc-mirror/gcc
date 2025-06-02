-- C41303F.ADA

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
--                 ACC ACC REC  ||             |
--                --------------||-------------|--------------------
--       1 '.ALL'  ACC ACC ARR  ||  XXXXXXXXX  |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ========================||=============|====================
--                 ACC ACC REC  ||             |
--                --------------||-------------|--------------------
--       2 '.ALL'  ACC ACC ARR  ||             |
--                --------------||-------------|--------------------
--                 ACC ACC SCLR ||             |
--      ============================================================


-- RM  1/20/82
-- RM  1/25/82
-- SPS 12/2/82


WITH REPORT;
USE REPORT;
PROCEDURE C41303F IS

BEGIN

     TEST ( "C41303F" , "CHECK THAT THE NOTATION  L.ALL  IS ALLOWED IF"
                    &   "  L  IS THE NAME OF AN ACCESS OBJECT"
                    &   " DESIGNATING  ANOTHER ACCESS OBJECT" );


     -------------------------------------------------------------------
     ---------------  ACCESS TO ACCESS TO ARRAY  -----------------------

     DECLARE

          TYPE  ARR     IS  ARRAY(1..2) OF BOOLEAN ;
          TYPE  ACCARR  IS  ACCESS ARR ;

          ACCARR_CONST    :  ACCARR  :=  NEW ARR'( TRUE , FALSE );
          ACCARR_VAR      :  ACCARR  :=    ACCARR_CONST  ;
          ACCARR_CONST2   :  ACCARR  :=  NEW ARR'( FALSE , TRUE );

          TYPE  ACC_ACCARR  IS  ACCESS ACCARR ;

          ACC_ACCARR_VAR  :  ACC_ACCARR :=  NEW ACCARR'(ACCARR_CONST2);

     BEGIN

          ACCARR_VAR          :=  ACC_ACCARR_VAR.ALL ;

          IF  ACCARR_VAR /= ACCARR_CONST2
          THEN
               FAILED( "ACC2 ARRAY, RIGHT SIDE OF ASSIGN., WRONG VAL.");
          END IF;


          ACC_ACCARR_VAR.ALL  :=  ACCARR_CONST ;

          IF  ACCARR_CONST /= ACC_ACCARR_VAR.ALL
          THEN
               FAILED( "ACC2 ARRAY, LEFT SIDE OF ASSIGN., WRONG VAL." );
          END IF;


     END ;

     -------------------------------------------------------------------

     RESULT;


END C41303F;
