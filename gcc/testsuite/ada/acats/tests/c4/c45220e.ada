-- C45220E.ADA

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
-- CHECK THE PROPER OPERATION OF THE MEMBERSHIP OPERATORS  'IN'  AND
--    'NOT IN'  FOR BOOLEAN TYPES.


-- RM 03/20/81
-- SPS 10/26/82


WITH REPORT;
PROCEDURE  C45220E  IS

     USE  REPORT ;

BEGIN

     TEST( "C45220E" , "CHECK THE PROPER OPERATION OF THE MEMBERSHIP" &
                       " OPERATORS  'IN'  AND  'NOT IN'  FOR" &
                       " BOOLEAN TYPES" );

     DECLARE

          SUBTYPE  SUBBOOL   IS  BOOLEAN RANGE FALSE..TRUE ;

          VAR  :            BOOLEAN  :=  FALSE ;
          CON  :   CONSTANT BOOLEAN  :=  FALSE ;

     BEGIN

          IF   TRUE NOT IN SUBBOOL  OR
               VAR  NOT IN SUBBOOL  OR
               CON  NOT IN SUBBOOL
          THEN
               FAILED( "WRONG VALUES FOR 'IN SUBBOOL'" );
          END IF;

          IF   FALSE   IN TRUE..FALSE  OR
               VAR NOT IN FALSE..TRUE  OR
               CON     IN TRUE..TRUE
          THEN
               FAILED( "WRONG VALUES FOR 'IN AAA..BBB'" );
          END IF;


          RESULT ;


     END ;


END C45220E ;
