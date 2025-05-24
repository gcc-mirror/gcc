-- C35508H.ADA

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
--     CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULTS WHEN THE
--     PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER IS A
--     BOOLEAN TYPE.

-- HISTORY:
--     RJW 03/24/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35508H IS

BEGIN
     TEST ("C35508H", "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "FORMAL DISCRETE TYPE WHOSE ACTUAL PARAMETER " &
                      "IS A BOOLEAN TYPE" );

     DECLARE

          TYPE NEWBOOL IS NEW BOOLEAN;

          GENERIC
               TYPE BOOL IS (<>);
               F, T : BOOL;
          PROCEDURE P (STR : STRING);

          PROCEDURE P (STR : STRING) IS
               SUBTYPE SBOOL IS BOOL RANGE T .. T;
          BEGIN
               BEGIN
                    IF BOOL'PRED (T) /= F THEN
                         FAILED ( "INCORRECT VALUE FOR " &
                                   STR & "'PRED OF T" );
                    END IF;
                    IF BOOL'SUCC (F) /= T THEN
                         FAILED ( "INCORRECT VALUE FOR " &
                                   STR & "'SUCC OF F" );
                    END IF;
               END;

               BEGIN
                    IF SBOOL'PRED (T) /= F THEN
                         FAILED ( "INCORRECT VALUE FOR SBOOL'PRED " &
                                  "OF T FOR " & STR);
                    END IF;
               END;

               BEGIN
                    IF SBOOL'PRED (SBOOL'BASE'FIRST) = T THEN
                         FAILED("'PRED('FIRST) WRAPPED AROUND " &
                                "TO TRUE FOR " & STR);
                    END IF;
                    FAILED ( "NO EXCEPTION RAISED FOR " &
                              STR & "'PRED (SBOOL'BASE'FIRST)" );
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " &
                                   STR & "'PRED (SBOOL'BASE'FIRST)" );
               END;

               BEGIN
                    IF SBOOL'SUCC (SBOOL'BASE'LAST) = F THEN
                         FAILED("'SUCC('LAST) WRAPPED AROUND TO " &
                                "FALSE FOR " & STR);
                    END IF;
                    FAILED ( "NO EXCEPTION RAISED FOR " & STR &
                             "'SUCC (SBOOL'BASE'LAST)" );
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED FOR " &
                                  STR & "'SUCC (SBOOL'BASE'LAST)" );
               END;
          END P;

          PROCEDURE NP1 IS NEW P
                         ( BOOL => BOOLEAN, F => FALSE, T => TRUE );

          PROCEDURE NP2 IS NEW P
                         ( BOOL => NEWBOOL, F => FALSE, T => TRUE );
     BEGIN
          NP1 ("BOOLEAN");
          NP2 ("NEWBOOL");
     END;

     RESULT;
END C35508H;
