-- C35505F.ADA

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
-- OBJECTIVE:
--     CHECK THAT CONSTRAINT ERROR IS RAISED BY THE ATTRIBUTES
--     'PRED' AND 'SUCC' WHEN THE PREFIX IS A CHARACTER TYPE
--     AND THE RESULT IS OUTSIDE OF THE BASE TYPE.

-- HISTORY:
--     JET 08/18/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE  C35505F  IS

     TYPE CHAR IS ('A', B);

     TYPE NEWCHAR IS NEW CHAR;

     FUNCTION IDENT (CH : CHAR) RETURN CHAR IS
     BEGIN
          RETURN CHAR'VAL (IDENT_INT (CHAR'POS (CH)));
     END;

     FUNCTION IDENT (CH : NEWCHAR) RETURN NEWCHAR IS
     BEGIN
          RETURN NEWCHAR'VAL (IDENT_INT (NEWCHAR'POS (CH)));
     END;

BEGIN

     TEST( "C35505F" , "CHECK THAT CONSTRAINT ERROR IS RAISED BY " &
                       "THE ATTRIBUTES 'PRED' AND 'SUCC' WHEN THE " &
                       "PREFIX IS A CHARACTER TYPE AND THE RESULT " &
                       "IS OUTSIDE OF THE BASE TYPE" );

     BEGIN
          IF CHAR'PRED (IDENT ('A')) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'PRED (IDENT ('A')) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'PRED (IDENT ('A')) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHAR'PRED (IDENT ('A'))" );
     END;

     BEGIN
          IF CHAR'SUCC (IDENT (B)) = B THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'SUCC (IDENT (B)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'SUCC (IDENT (B)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHAR'SUCC (IDENT (B))" );
     END;

     BEGIN
          IF NEWCHAR'PRED (IDENT ('A')) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR NEWCHAR'PRED (IDENT ('A')) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR NEWCHAR'PRED (IDENT ('A')) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR NEWCHAR'PRED (IDENT ('A'))" );
     END;

     BEGIN
          IF NEWCHAR'SUCC (IDENT (B)) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR NEWCHAR'SUCC (IDENT (B)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR NEWCHAR'SUCC (IDENT (B)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR NEWCHAR'SUCC (IDENT (B))" );
     END;

     BEGIN
          IF CHARACTER'PRED (IDENT_CHAR (CHARACTER'BASE'FIRST)) = 'A'
             THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHARACTER'PRED " &
                        "(IDENT_CHAR (CHARACTER'BASE'FIRST)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHARACTER'PRED " &
                        "(IDENT_CHAR (CHARACTER'BASE'FIRST)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'PRED " &
                        "(IDENT_CHAR (CHARACTER'BASE'FIRST))" );
     END;

     BEGIN
          IF CHARACTER'SUCC (IDENT_CHAR (CHARACTER'BASE'LAST)) = 'Z'
             THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHARACTER'SUCC " &
                        "(IDENT_CHAR (CHARACTER'BASE'LAST)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHARACTER'SUCC " &
                        "(IDENT_CHAR (CHARACTER'BASE'LAST)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'SUCC " &
                        "(IDENT_CHAR (CHARACTER'BASE'LAST))" );
     END;

     RESULT;

END C35505F;
