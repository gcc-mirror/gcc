-- C47004A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES AN INTEGER 
-- TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE VALUE OF THE 
-- OPERAND DOES NOT LIE WITHIN THE RANGE OF THE TYPE MARK.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47004A IS

BEGIN

     TEST( "C47004A", "WHEN THE TYPE MARK IN A QUALIFIED " &
                      "EXPRESSION DENOTES AN INTEGER " &
                      "TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "WHEN THE VALUE OF THE OPERAND DOES NOT LIE " &
                      "WITHIN THE RANGE OF THE TYPE MARK" );

     DECLARE  
          
          TYPE INT IS RANGE -10 .. 10;
          SUBTYPE SINT IS INT RANGE -5 .. 5;
          
          FUNCTION IDENT (I : INT) RETURN INT IS
          BEGIN
               RETURN INT (IDENT_INT (INTEGER (I)));
          END;

     BEGIN
          IF SINT'(IDENT (10)) = 5 THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SINT - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SINT - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE SINT" );
     END;

     DECLARE  
          
          SUBTYPE SINTEGER IS INTEGER RANGE -10 .. 10;
          
     BEGIN
          IF SINTEGER'(IDENT_INT (20)) = 15 THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SINTEGER - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SINTEGER - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE SINTEGER" );
     END;

     DECLARE  
          
          TYPE NINTEGER IS NEW INTEGER;
          SUBTYPE SNINT IS NINTEGER RANGE -10 .. 10;
          
          FUNCTION IDENT (I : NINTEGER) RETURN NINTEGER IS
          BEGIN
               RETURN NINTEGER (IDENT_INT (INTEGER (I)));
          END;

     BEGIN
          IF SNINT'(IDENT (-20)) = -10 THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SNINT - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE SNINT - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE SNINT" );
     END;

     RESULT;
END C47004A;
