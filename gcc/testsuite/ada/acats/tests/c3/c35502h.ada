-- C35502H.ADA

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
-- CHECK THAT 'PRED' AND 'SUCC' YIELD THE CORRECT RESULTS WHEN 
-- THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL ARGUMENT IS
-- AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR A CHARACTER TYPE.

-- RJW 5/27/86

WITH REPORT; USE REPORT;

PROCEDURE C35502H IS

          TYPE ENUM IS (A, BC, ABC, A_B_C, ABCD);

          TYPE NEWENUM IS NEW ENUM;

BEGIN
     TEST ("C35502H", "CHECK THAT 'PRED' AND 'SUCC' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS A " &
                      "FORMAL DISCRETE TYPE WHOSE ACTUAL " &
                      "ARGUMENT IS AN ENUMERATION TYPE OTHER THAN " &
                      "A CHARACTER OR A BOOLEAN TYPE" );

     DECLARE
          GENERIC
               TYPE E IS (<>);
               STR : STRING;
          PROCEDURE P;
          
          PROCEDURE P IS
               SUBTYPE SE IS E RANGE E'VAL(0) .. E'VAL(1);
          BEGIN
               FOR I IN E'VAL (1) .. E'VAL (4) LOOP
                    IF SE'PRED (I) /= 
                       E'VAL (E'POS (I) - 1) THEN
                         FAILED ("INCORRECT " & STR & "'PRED(" &
                                  E'IMAGE (I) & ")" );
                    END IF;
               END LOOP;

               FOR I IN E'VAL (0) .. E'VAL (3) LOOP
                    IF SE'SUCC (I) /= 
                       E'VAL (E'POS (I) + 1) THEN
                         FAILED ("INCORRECT " & STR & "'SUCC(" &
                                  E'IMAGE (I) & ")" );
                    END IF;
               END LOOP;

          END P;

          PROCEDURE PE IS NEW P ( ENUM, "ENUM" );
          PROCEDURE PN IS NEW P ( NEWENUM, "NEWENUM" );

     BEGIN
          PE;
          PN;
     END;
          
     RESULT;
END C35502H;
