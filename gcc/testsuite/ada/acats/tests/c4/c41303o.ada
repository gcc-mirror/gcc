-- C41303O.ADA

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
--                 ACC SCLR     ||             |     XXXXXXXXX
--      ========================||=============|====================
--                 ACC ACC REC  ||             |
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


-- RM  1/27/82
-- SPS 12/2/82


WITH REPORT;
USE REPORT;
PROCEDURE C41303O IS


BEGIN

     TEST ( "C41303O" , "CHECK THAT  L.ALL  , WHERE  L  IS THE NAME OF"
                      & " AN ACCESS OBJECT DESIGNATING A RECORD, AN"
                      & " ARRAY, OR A SCALAR,  IS ALLOWED AS"
                      & " ACTUAL PARAMETER OF ANY MODE" );


     -------------------------------------------------------------------
     --------------------  ACCESS TO SCALAR  ---------------------------

     DECLARE

          TYPE  NEWINT  IS  NEW INTEGER ;

          NEWINT_CONST    :  NEWINT  :=  813 ;
          NEWINT_VAR      :  NEWINT  :=  NEWINT_CONST  ;
          NEWINT_VAR0     :  NEWINT  :=  NEWINT_CONST  ;

          TYPE  ACC_NEWINT  IS  ACCESS NEWINT ;

          ACC_NEWINT_VAR  :  ACC_NEWINT  :=  NEW NEWINT'( 707 );
          ACC_NEWINT_VAR0 :  ACC_NEWINT  :=  NEW NEWINT'( 707 );


          PROCEDURE  R_ASSIGN( R_IN    : IN      NEWINT ;
                               R_INOUT : IN OUT  NEWINT )  IS
          BEGIN
               NEWINT_VAR   :=  R_IN    ;
               NEWINT_VAR0  :=  R_INOUT ;
          END ;


          PROCEDURE  L_ASSIGN( L_OUT   :    OUT  NEWINT ;
                               L_INOUT : IN OUT  NEWINT )  IS
          BEGIN
               L_OUT   :=  NEWINT_CONST ;
               L_INOUT :=  NEWINT_CONST ;
          END ;


     BEGIN

          R_ASSIGN( ACC_NEWINT_VAR.ALL , ACC_NEWINT_VAR0.ALL );

          IF  NEWINT_VAR /= ( 707 )
          THEN
               FAILED( "ACC. NEWINT, RIGHT SIDE (1), WRONG VAL." );
          END IF;

          IF  NEWINT_VAR0 /= ( 707 )
          THEN
               FAILED( "ACC. NEWINT, RIGHT SIDE (2), WRONG VAL." );
          END IF;


          L_ASSIGN( ACC_NEWINT_VAR.ALL , ACC_NEWINT_VAR0.ALL );

          IF  ACC_NEWINT_VAR.ALL /= 813 
          THEN
               FAILED( "ACC. NEWINT, LEFT SIDE (1), WRONG VAL." );
          END IF;

          IF  ACC_NEWINT_VAR0.ALL /= 813 
          THEN
               FAILED( "ACC. NEWINT, LEFT SIDE (2), WRONG VAL." );
          END IF;


     END ;

     -------------------------------------------------------------------

     RESULT;


END C41303O;
