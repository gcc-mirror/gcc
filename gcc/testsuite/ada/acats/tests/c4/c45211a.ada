-- C45211A.ADA

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
-- CHECK MEMBERSHIP TESTS FOR AN 'UNNATURAL' ORDERING OF CHARACTER
-- LITERALS.

-- RJW 1/22/86

WITH REPORT; USE REPORT;
PROCEDURE C45211A IS

     TYPE  T  IS  ( 'S' , 'Q' , 'P' , 'M' , 'R' );
     SUBTYPE ST IS T RANGE 'P' .. 'R';

     MVAR  : T := T'('M') ;
     QVAR  : T := T'('Q') ;
     MCON  : CONSTANT T := T'('M');
     QCON  : CONSTANT T := T'('Q');

BEGIN

     TEST( "C45211A" , "CHECK MEMBERSHIP TESTS FOR AN 'UNNATURAL' " &
                       "ORDERING OF CHARACTER LITERALS" ) ;

     IF QVAR IN T'('P') .. T'('R')  OR
        'Q'  IN ST
     THEN
          FAILED ( "MEMBERSHIP TEST FOR 'UNNATURAL' ORDERING - 1" );
     END IF;

     IF MVAR NOT IN T'('P') .. T'('R')  OR
        'M'  NOT IN ST
     THEN
          FAILED ( "MEMBERSHIP TEST FOR 'UNNATURAL' ORDERING - 2" );
     END IF;

     IF QCON IN T'('P') .. T'('R')  OR
        MCON NOT IN ST
     THEN
          FAILED ( "MEMBERSHIP TEST FOR 'UNNATURAL' ORDERING - 3" );
     END IF;

     RESULT;

END C45211A;
