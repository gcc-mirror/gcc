-- C35505C.ADA

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
--     IF THE RETURNED VALUES WOULD BE OUTSIDE OF THE BASE TYPE,
--     WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT
--     IS A USER-DEFINED ENUMERATION TYPE.

-- HISTORY:
--     RJW 06/05/86  CREATED ORIGINAL TEST.
--     VCL 08/19/87  REMOVED THE FUNCTION 'IDENT' IN THE GENERIC
--                   PROCEDURE 'P' AND REPLACED ALL CALLS TO 'IDENT'
--                   WITH "T'VAL(IDENT_INT(T'POS(...)))".

WITH REPORT; USE REPORT;

PROCEDURE C35505C IS

     TYPE B IS ('Z', 'X', Z, X);

     SUBTYPE C IS B RANGE 'X' .. Z;

BEGIN
     TEST ( "C35505C", "CHECK THAT 'SUCC' AND 'PRED' RAISE " &
                       "CONSTRAINT_ERROR APPROPRIATELY WHEN THE " &
                       "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
                       "ARGUMENT IS A USER-DEFINED ENUMERATION TYPE" );

     DECLARE
          GENERIC
               TYPE T IS (<>);
               STR : STRING;
          PROCEDURE P;

          PROCEDURE P IS

          BEGIN
               BEGIN
                    IF T'PRED (T'VAL (IDENT_INT (T'POS
                              (T'BASE'FIRST)))) = T'FIRST THEN
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
                    IF T'SUCC (T'VAL (IDENT_INT (T'POS
                              (T'BASE'LAST)))) = T'LAST THEN
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
          END P;

          PROCEDURE PB IS NEW P (B, "B");
          PROCEDURE PC IS NEW P (C, "C");
     BEGIN
          PB;
          PC;
     END;
RESULT;
END C35505C;
