-- C45274B.ADA

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
--   * RECORD TYPES WITHOUT DISCRIMINANTS;
--   * PRIVATE TYPES WITHOUT DISCRIMINANTS;
--   * LIMITED PRIVATE TYPES WITHOUT DISCRIMINANTS;
-->> * (UNCONSTRAINED) RECORD TYPES WITH DISCRIMINANTS; 
-->> * (UNCONSTRAINED) PRIVATE TYPES WITH DISCRIMINANTS;
-->> * (UNCONSTRAINED) LIMITED PRIVATE TYPES WITH DISCRIMINANTS.


-- RM  3/03/82


WITH REPORT;
USE REPORT;
PROCEDURE C45274B IS


BEGIN

     TEST ( "C45274B" , "CHECK THAT THE MEMBERSHIP OPERATOR  IN " &
                        "  ( NOT IN )  YIELDS  TRUE   (RESP.  FALSE )" &
                        " FOR UNCONSTRAINED TYPES WITH DISCRIMINANTS" );


     -------------------------------------------------------------------
     --------  UNCONSTRAINED RECORD TYPES WITH DISCRIMINANTS  ----------

     DECLARE

          TYPE  REC ( DISCR : BOOLEAN ) IS
               RECORD
                    A , B : INTEGER ;
               END RECORD ;

          X : REC(FALSE) := ( FALSE , 19 , 81 );

          TYPE  REC0 ( DISCR : BOOLEAN := FALSE ) IS
               RECORD
                    A , B : INTEGER ;
               END RECORD ;

          Y : REC0 := ( TRUE , 19 , 81 );

     BEGIN

          IF  X  IN  REC  THEN
               NULL;
          ELSE 
               FAILED( "WRONG VALUE: 'IN', 1A" );
          END IF;

          IF  Y  NOT IN  REC0  THEN
               FAILED( "WRONG VALUE: 'NOT IN', 1B" );
          ELSE 
               NULL;
          END IF;

     EXCEPTION

          WHEN  OTHERS =>
               FAILED( "1 -  'IN'  ( 'NOT IN' )  RAISED AN EXCEPTION");

     END;


     -------------------------------------------------------------------
     -------  UNCONSTRAINED PRIVATE TYPES WITH DISCRIMINANTS  ----------

     DECLARE

          PACKAGE  P  IS
               TYPE  PRIV ( DISCR : BOOLEAN ) IS PRIVATE;
          PRIVATE
               TYPE  PRIV ( DISCR : BOOLEAN ) IS
                    RECORD
                         A , B : INTEGER ;
                    END RECORD ;
          END  P ;

          USE  P ;

          X : PRIV(FALSE) ;

          PACKAGE BODY  P  IS
          BEGIN
               X := ( FALSE , 19 , 91 );
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
     ---------  UNCONSTRAINED LIM. PRIV. TYPES WITH DISCRIM.  ----------

     DECLARE

          PACKAGE  P  IS
               TYPE  LP ( DISCR : BOOLEAN := FALSE ) IS LIMITED PRIVATE;
          PRIVATE
               TYPE  LP ( DISCR : BOOLEAN := FALSE ) IS
                    RECORD
                         A , B : INTEGER ;
                    END RECORD ;
          END  P ;

          USE  P ;

          X : LP(TRUE) ;

          PACKAGE BODY  P  IS
          BEGIN
               X := ( TRUE , 19 , 91 );
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
               TYPE  LP ( DISCR : BOOLEAN := FALSE ) IS LIMITED PRIVATE;
          PRIVATE
               TYPE  LP ( DISCR : BOOLEAN := FALSE ) IS
                    RECORD
                         A , B : INTEGER ;
                    END RECORD ;
          END  P ;

          USE  P ;

          Y : LP(TRUE) ;

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


END  C45274B ;
