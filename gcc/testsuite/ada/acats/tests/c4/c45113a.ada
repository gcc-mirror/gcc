-- C45113A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE OPERANDS OF LOGICAL
-- OPERATORS HAVE DIFFERENT LENGTHS.

-- RJW  1/15/86

WITH REPORT; USE REPORT; 

PROCEDURE C45113A IS

BEGIN

     TEST( "C45113A" , "CHECK ON LOGICAL OPERATORS WITH " &
                       "OPERANDS OF DIFFERENT LENGTHS" );
     
     DECLARE

          TYPE ARR IS ARRAY ( INTEGER RANGE <> ) OF BOOLEAN;
     
          A : ARR( IDENT_INT(1) .. IDENT_INT(2) ) := ( TRUE, FALSE );
          B : ARR( IDENT_INT(1) .. IDENT_INT(3) ) := ( TRUE, FALSE, 
                                                       TRUE );

     BEGIN

          BEGIN -- TEST FOR 'AND'.
               IF (A AND B) = B THEN
                    FAILED ( "A AND B = B" );
               END IF;
               FAILED ( "NO EXCEPTION RAISED FOR 'AND'" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR 'AND'" );
          END;           


          BEGIN -- TEST FOR 'OR'.
               IF (A OR B) = B THEN
                    FAILED ( "A OR B = B" );
               END IF;
               FAILED ( "NO EXCEPTION RAISED FOR 'OR'" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR 'OR'" );
          END;           


          BEGIN -- TEST FOR 'XOR'.
               IF (A XOR B) = B THEN
                    FAILED ( "A XOR B = B" );
               END IF;
               FAILED ( "NO EXCEPTION RAISED FOR 'XOR'" );
          EXCEPTION
               WHEN CONSTRAINT_ERROR => 
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED FOR 'XOR'" );
          END;           

     END;

     RESULT;

END C45113A;
