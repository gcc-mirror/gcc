-- C35503L.ADA

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
--     CHECK THAT 'POS' AND 'VAL' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER
--     IS AN INTEGER TYPE.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35503L IS

BEGIN
     TEST ("C35503L", "CHECK THAT 'POS' AND 'VAL' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "GENERIC FORMAL DISCRETE TYPE WHOSE " &
                      "ACTUAL PARAMETER IS AN INTEGER TYPE" );

     DECLARE
          TYPE INTRANGE IS RANGE -6 .. 6;

          GENERIC
               TYPE INT IS (<>);
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               SUBTYPE SINT IS INT RANGE
                   INT'VAL (IDENT_INT(-4)) .. INT'VAL (IDENT_INT(4));
               I :INTEGER;
          BEGIN
               I := IDENT_INT(-6);
               FOR S IN INT'VAL (IDENT_INT(-6)) ..
                                                 INT'VAL (IDENT_INT(6))
               LOOP
                    BEGIN
                         IF SINT'POS (S) /= I THEN
                              FAILED ( "WRONG VALUE FOR " &
                                        STR & "'POS OF "
                                       & INT'IMAGE (S) );
                         END IF;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ( "EXCEPTION RAISED FOR " &
                                        STR & "'POS "
                                       & "OF " & INT'IMAGE (S) );
                    END;
                    BEGIN
                         IF SINT'VAL (I) /= S THEN
                              FAILED ( "WRONG VALUE FOR " &
                                        STR & "'VAL "
                                       & "OF " & INT'IMAGE (S) );
                         END IF;
                    EXCEPTION
                         WHEN OTHERS =>
                              FAILED ( "EXCEPTION RAISED FOR " &
                                        STR & "'VAL "
                                       & "OF " & INT'IMAGE (S) );
                    END;
                    I := I + 1;
               END LOOP;
          END P;

          PROCEDURE P1 IS NEW P (INTRANGE);
          PROCEDURE P2 IS NEW P (INTEGER);

     BEGIN
          P1 ("INTRANGE");
          P2 ("INTEGER");
     END;

     RESULT;

END C35503L;
