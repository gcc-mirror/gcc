-- C47003A.ADA

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
-- WHEN THE TYPE MARK IN A QUALIFIED EXPRESSION DENOTES AN 
-- ENUMERATION TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN THE 
-- VALUE OF THE OPERAND DOES NOT LIE WITHIN THE RANGE OF THE TYPE MARK.

-- RJW 7/23/86

WITH REPORT; USE REPORT; 
PROCEDURE C47003A IS

BEGIN

     TEST( "C47003A", "WHEN THE TYPE MARK IN A QUALIFIED " &
                      "EXPRESSION DENOTES AN ENUMERATION " &
                      "TYPE, CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                      "WHEN THE VALUE OF THE OPERAND DOES NOT LIE " &
                      "WITHIN THE RANGE OF THE TYPE MARK" );

     DECLARE  
          
          TYPE WEEK IS (SUN, MON, TUE, WED, THU, FRI, SAT);
          SUBTYPE MIDWEEK IS WEEK RANGE TUE .. THU;

          FUNCTION IDENT (W : WEEK) RETURN WEEK IS
          BEGIN
               RETURN WEEK'VAL (IDENT_INT (WEEK'POS (W)));
          END IDENT;

     BEGIN
          IF MIDWEEK'(IDENT (SUN)) = TUE THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE MIDWEEK - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE MIDWEEK - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE MIDWEEK" );
     END;

     DECLARE  
          
          SUBTYPE CHAR IS CHARACTER RANGE 'C' .. 'R';

     BEGIN
          IF CHAR'(IDENT_CHAR ('A')) = 'C' THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE CHAR - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE CHAR - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE CHAR" );
     END;

     DECLARE  

          TYPE NBOOL IS NEW BOOLEAN;
          SUBTYPE NFALSE IS NBOOL RANGE FALSE .. FALSE;

          FUNCTION IDENT (B : NBOOL) RETURN NBOOL IS
          BEGIN
               RETURN NBOOL (IDENT_BOOL (BOOLEAN (B)));
          END IDENT;

     BEGIN
          IF NFALSE'(IDENT (TRUE)) = FALSE THEN
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE NFALSE - 1");
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR VALUE OUTSIDE OF " &
                        "SUBTYPE NFALSE - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR VALUE OUTSIDE " &
                        "OF SUBTYPE NFALSE" );
     END;

     RESULT;
END C47003A;
