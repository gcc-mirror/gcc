-- C45114B.ADA

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
-- CHECK THAT LOGICAL OPERATORS ARE DEFINED FOR PACKED BOOLEAN ARRAYS.

--  RJW  1/17/86

WITH  REPORT; USE REPORT;
PROCEDURE  C45114B  IS

BEGIN

     TEST( "C45114B" , "CHECK THAT LOGICAL OPERATORS ARE DEFINED " &
                       "FOR PACKED BOOLEAN ARRAYS" );

     DECLARE

          TYPE ARR IS ARRAY (1 .. 32) OF BOOLEAN;

          PRAGMA PACK (ARR);

          A : ARR := ( TRUE, TRUE, FALSE, FALSE, OTHERS => TRUE );
          B : ARR := ( TRUE, FALSE, TRUE, FALSE, OTHERS => FALSE );

          A_AND_B : ARR := ( TRUE, OTHERS => FALSE );
          A_OR_B  : ARR := ARR'( 4 => FALSE, OTHERS => TRUE );
          A_XOR_B : ARR := ARR'( 1|4 => FALSE, OTHERS => TRUE );
          NOT_A   : ARR := ARR'( 3|4 => TRUE, OTHERS => FALSE );

     BEGIN

          IF ( A AND B ) /= A_AND_B THEN
               FAILED ( "'AND' NOT CORRECTLY DEFINED" );
          END IF;

          IF ( A OR B ) /= A_OR_B THEN
               FAILED ( "'OR' NOT CORRECTLY DEFINED" );
          END IF;

          IF ( A XOR B ) /= A_XOR_B THEN
               FAILED ( "'XOR' NOT CORRECTLY DEFINED" );
          END IF;

          IF NOT A /= NOT_A THEN
               FAILED ( "'NOT' NOT CORRECTLY DEFINED" );
          END IF;

     END;

     RESULT;

END C45114B;
