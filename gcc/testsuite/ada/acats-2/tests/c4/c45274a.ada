-- C45274A.ADA

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
-- CHECK THAT THE MEMBERSHIP OPERATOR  IN   ( NOT IN )  ALWAYS
--     YIELDS  TRUE   (RESP.  FALSE )  FOR
--
-->> * RECORD TYPES WITHOUT DISCRIMINANTS;
-->> * PRIVATE TYPES WITHOUT DISCRIMINANTS;
-->> * LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS;
--   * (UNCONSTRAINED) RECORD TYPES WITH DISCRIMINANTS; 
--   * (UNCONSTRAINED) PRIVATE TYPES WITH DISCRIMINANTS;
--   * (UNCONSTRAINED) LIMITED PRIVATE TYPES WITH DISCRIMINANTS.


-- RM  3/01/82


WITH REPORT;
USE REPORT;
PROCEDURE C45274A IS


BEGIN

     TEST ( "C45274A" , "CHECK THAT THE MEMBERSHIP OPERATOR  IN " &
                        "  ( NOT IN )  YIELDS  TRUE   (RESP.  FALSE )" &
                        " FOR RECORD TYPES WITHOUT DISCRIMINANTS," &
                        " PRIVATE TYPES WITHOUT DISCRIMINANTS, AND" &
                        " LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS");


     -------------------------------------------------------------------
     -----------------  RECORD TYPES WITHOUT DISCRIMINANTS  ------------

     DECLARE

          TYPE  REC  IS
               RECORD
                    A , B : INTEGER ;
               END RECORD ;

          X : REC := ( 19 , 91 );

     BEGIN

          IF  X  IN  REC  THEN
               NULL;
          ELSE 
               FAILED( "WRONG VALUE: 'IN', 1" );
          END IF;

          IF  X  NOT IN  REC  THEN
               FAILED( "WRONG VALUE: 'NOT IN', 1" );
          ELSE 
               NULL;
          END IF;

     EXCEPTION

          WHEN  OTHERS =>
               FAILED( "1 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

     END;


     -------------------------------------------------------------------
     -----------------  PRIVATE TYPES WITHOUT DISCRIMINANTS  -----------

     DECLARE

          PACKAGE  P  IS
               TYPE  PRIV  IS PRIVATE;
          PRIVATE
               TYPE  PRIV  IS
                    RECORD
                         A , B : INTEGER ;
                    END RECORD ;
          END  P ;

          USE  P ;

          X : PRIV ;

          PACKAGE BODY  P  IS
          BEGIN
               X := ( 19 , 91 );
          END  P ;

     BEGIN

          IF  X  IN  PRIV  THEN
               NULL;
          ELSE 
               FAILED( "WRONG VALUE: 'IN', 2" );
          END IF;

          IF  X  NOT IN  PRIV  THEN
               FAILED( "WRONG VALUE: 'NOT IN', 2" );
          ELSE 
               NULL;
          END IF;

     EXCEPTION

          WHEN  OTHERS =>
               FAILED( "2 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

     END;

     -------------------------------------------------------------------
     ---------  LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS  -----------

     DECLARE

          PACKAGE  P  IS
               TYPE  LP  IS LIMITED PRIVATE;
          PRIVATE
               TYPE  LP  IS
                    RECORD
                         A , B : INTEGER ;
                    END RECORD ;
          END  P ;

          USE  P ;

          X : LP ;

          PACKAGE BODY  P  IS
          BEGIN
               X := ( 19 , 91 );
          END  P ;

     BEGIN

          IF  X  IN  LP  THEN
               NULL;
          ELSE 
               FAILED( "WRONG VALUE: 'IN', 3" );
          END IF;

          IF  X  NOT IN  LP  THEN
               FAILED( "WRONG VALUE: 'NOT IN', 3" );
          ELSE 
               NULL;
          END IF;

     EXCEPTION

          WHEN  OTHERS =>
               FAILED( "3 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

     END;

     -------------------------------------------------------------------

     DECLARE

          PACKAGE  P  IS
               TYPE  LP  IS LIMITED PRIVATE;
          PRIVATE
               TYPE  LP  IS
                    RECORD
                         A , B : INTEGER ;
                    END RECORD ;
          END  P ;

          USE  P ;

          Y : LP ;

     -- CHECK THAT NO EXCEPTION FOR UNINITIALIZED VARIABLE
     BEGIN

          IF  Y  IN  LP  THEN
               NULL;
          ELSE 
               FAILED( "WRONG VALUE: 'IN', 3BIS" );
          END IF;

          IF  Y  NOT IN  LP  THEN
               FAILED( "WRONG VALUE: 'NOT IN', 3BIS" );
          ELSE 
               NULL;
          END IF;

     EXCEPTION

          WHEN  OTHERS =>
               FAILED( "3BIS - UNINITIALIZED VARIABLE - 'IN' " &
                       "( 'NOT IN' )  RAISED AN EXCEPTION" );

     END;


     -------------------------------------------------------------------


     RESULT;


END  C45274A ;
