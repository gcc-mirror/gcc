-- C45202B.ADA

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
-- CHECK MEMBERSHIP OPERATIONS IN THE CASE IN WHICH A USER HAS 
-- REDEFINED THE ORDERING OPERATORS.

-- RJW 1/22/86

WITH REPORT; USE REPORT;

PROCEDURE C45202B IS


BEGIN

     TEST( "C45202B" , "CHECK MEMBERSHIP OPERATIONS IN WHICH A USER " &
                       "HAS REDEFINED THE ORDERING OPERATORS" ) ;


     DECLARE

          TYPE  T  IS  ( AA, BB, CC, LIT, XX, YY, ZZ );    
          SUBTYPE ST IS T RANGE AA .. LIT;     

          VAR  :           T :=  LIT ;
          CON  :  CONSTANT T :=  LIT ;

          FUNCTION ">" ( L, R : T ) RETURN BOOLEAN IS
          BEGIN
               RETURN T'POS(L) <= T'POS(R);
          END;
                    
          FUNCTION ">=" ( L, R : T ) RETURN BOOLEAN IS
          BEGIN
               RETURN T'POS(L) < T'POS(R);
          END;

          FUNCTION "<" ( L, R : T ) RETURN BOOLEAN IS
          BEGIN
               RETURN T'POS(L) >= T'POS(R);
          END;
                    
          FUNCTION "<=" ( L, R : T ) RETURN BOOLEAN IS
          BEGIN
               RETURN T'POS(L) > T'POS(R);
          END;
                    

     BEGIN

          IF   LIT NOT IN ST      OR
               VAR NOT IN ST      OR
               CON NOT IN ST      OR
               NOT (VAR IN ST)    OR
               XX IN ST           OR
               NOT (XX NOT IN ST) 
          THEN
               FAILED( "WRONG VALUES FOR 'IN ST'" );
          END IF;

          IF   LIT     IN AA ..CC       OR
               VAR NOT IN LIT..ZZ       OR
               CON     IN ZZ ..AA       OR
               NOT (CC IN CC .. YY)     OR
               NOT (BB NOT IN CC .. YY) 
          THEN
               FAILED( "WRONG VALUES FOR 'IN AA..CC'" );
          END IF;

     END;

     RESULT;

END C45202B;
