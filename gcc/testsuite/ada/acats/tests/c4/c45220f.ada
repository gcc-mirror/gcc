-- C45220F.ADA

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
-- CHECK THAT THE MEMBERSHIP OPERATIONS WORK CORRECTLY FOR DERIVED
-- BOOLEAN TYPES.

-- GLH 08/01/85

WITH REPORT;
PROCEDURE  C45220F  IS

     USE  REPORT ;

BEGIN

     TEST( "C45220F" , "CHECK MEMBERSHIP OPERATIONS FOR " &
                       "DERIVED BOOLEAN");

     DECLARE

          TYPE NEWBOOL IS NEW BOOLEAN;

          VAR  :          NEWBOOL  :=  FALSE ;
          CON  : CONSTANT NEWBOOL  :=  FALSE ;

     BEGIN

          IF   TRUE NOT IN NEWBOOL  OR
               VAR  NOT IN NEWBOOL  OR
               CON  NOT IN NEWBOOL
          THEN
               FAILED( "WRONG VALUES FOR 'IN NEWBOOL'" );
          END IF;

          IF  NEWBOOL'(FALSE)   IN TRUE..FALSE  OR
              VAR           NOT IN FALSE..TRUE  OR
              CON               IN TRUE..TRUE
          THEN
               FAILED( "WRONG VALUES FOR 'IN AAA..BBB'" );
          END IF;

          RESULT ;

     END ;

END C45220F ;
