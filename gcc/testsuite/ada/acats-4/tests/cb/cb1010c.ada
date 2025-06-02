-- CB1010C.ADA

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
-- CHECK THAT STORAGE_ERROR IS RAISED WHEN STORAGE FOR A DECLARATIVE
-- ITEM IS INSUFFICIENT.

-- JRK 8/30/85

WITH REPORT; USE REPORT;

PROCEDURE CB1010C IS

     N : INTEGER := IDENT_INT (1000);
     M : INTEGER := IDENT_INT (0);

     PROCEDURE OVERFLOW_STACK IS
     BEGIN
          N := N + M;
          DECLARE
               A : ARRAY (1 .. N) OF INTEGER;
          BEGIN
               A (N) := M;
               IF N > M THEN  -- ALWAYS TRUE.
                    OVERFLOW_STACK;
               END IF;
               M := A (N);    -- TO PREVENT TAIL RECURSION OPTIMIZATION.
          END;
     END OVERFLOW_STACK;

BEGIN
     TEST ("CB1010C", "CHECK THAT STORAGE_ERROR IS RAISED WHEN " &
                      "STORAGE FOR A DECLARATIVE ITEM IS INSUFFICIENT");

     BEGIN

          OVERFLOW_STACK;
          FAILED ("EXCEPTION NOT RAISED BY STACK OVERFLOW");

     EXCEPTION
          WHEN STORAGE_ERROR =>
               IF N /= 1000 OR M /= 0 THEN
                    FAILED ("VALUES OF VARIABLES N OR M WERE ALTERED");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY STACK OVERFLOW");
     END;

     RESULT;
END CB1010C;
