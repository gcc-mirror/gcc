-- C35505E.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR 'SUCC' AND 'PRED',
--     IF THE RESULT WOULD BE OUTSIDE THE RANGE OF THE BASE TYPE,
--     WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT
--     IS TYPE CHARACTER OR A SUBTYPE OF TYPE CHARACTER.

-- HISTORY:
--     DWC 07/01/87

WITH REPORT; USE REPORT;

PROCEDURE C35505E IS

     TYPE CHAR IS ('A', B, C);
     SUBTYPE NEWCHAR IS CHAR;

BEGIN
     TEST ( "C35505E", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR " &
                       "'SUCC' AND 'PRED', IF THE RESULT WOULD BE " &
                       "OUTSIDE THE RANGE OF THE BASE TYPE, WHEN " &
                       "THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
                       "ACTUAL ARGUMENT IS A CHARACTER TYPE ");

     DECLARE
          GENERIC
               TYPE SUBCH IS (<>);
               STR : STRING;
               I1, I2 : INTEGER;
          PROCEDURE P;

          PROCEDURE P IS

               FUNCTION IDENT (C : SUBCH) RETURN SUBCH IS
               BEGIN
                    RETURN SUBCH'VAL (IDENT_INT (SUBCH'POS (C)));
               END IDENT;

          BEGIN
               BEGIN
                    IF SUBCH'PRED (SUBCH'BASE'FIRST) = SUBCH'VAL (0)
                         THEN
                         FAILED ( "CONSTRAINT_ERROR NOT RAISED FOR " &
                                   STR & "'PRED -  1" );
                    ELSE
                         FAILED ( "CONSTRAINT_ERROR NOT RAISED FOR " &
                                   STR & "'PRED -  2" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " &
                                   STR & "'PRED - 1" );
               END;

               BEGIN
                    IF SUBCH'SUCC (SUBCH'BASE'LAST) = SUBCH'VAL (0) THEN
                         FAILED ( "CONSTRAINT_ERROR NOT RAISED FOR " &
                                   STR & "'SUCC -  1" );
                    ELSE
                         FAILED ( "CONSTRAINT_ERROR NOT RAISED FOR " &
                                   STR & "'SUCC -  2" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " &
                                   STR & "'SUCC - 1" );
               END;

               BEGIN
                    IF SUBCH'PRED (IDENT (SUBCH'BASE'FIRST)) =
                       SUBCH'VAL (I1) THEN
                         FAILED ( "NO EXCEPTION RAISED " &
                                  "FOR " & STR & "'PRED " &
                                  "(IDENT (SUBCH'BASE'FIRST)) - 1" );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED " &
                                  "FOR " & STR & "'PRED " &
                                  "(IDENT (SUBCH'BASE'FIRST)) - 2" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED " &
                                  "FOR " & STR & "'PRED " &
                                  "(IDENT (SUBCH'BASE'FIRST))" );
               END;

               BEGIN
                    IF SUBCH'SUCC (IDENT(SUBCH'BASE'LAST)) =
                       SUBCH'VAL (I2) THEN
                         FAILED ( "NO EXCEPTION RAISED " &
                                  "FOR " & STR & "'SUCC " &
                                  "(IDENT (SUBCH'BASE'LAST)) - 1" );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED " &
                                  "FOR " & STR & "'SUCC " &
                                  "(IDENT (SUBCH'BASE'LAST)) - 2" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED " &
                                  "FOR " & STR & "'SUCC " &
                                  "(IDENT (SUBCH'BASE'LAST))" );
               END;
          END P;

          PROCEDURE PCHAR  IS NEW P (CHAR, "CHAR", 0, 1);
          PROCEDURE PNCHAR IS NEW P (NEWCHAR, "NEWCHAR", 0, 1);
     BEGIN
          PCHAR;
          PNCHAR;
     END;
RESULT;
END C35505E;
