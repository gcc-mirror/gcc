-- CB1010D.ADA

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
-- CHECK THAT STORAGE_ERROR IS RAISED WHEN STORAGE FOR THE EXECUTION OF
-- A SUBPROGRAM IS INSUFFICIENT.

-- PNH 8/26/85
-- JRK 8/30/85

WITH REPORT; USE REPORT;

PROCEDURE CB1010D IS

     N : INTEGER := IDENT_INT (1);
     M : INTEGER := IDENT_INT (0);

     PROCEDURE OVERFLOW_STACK IS
     BEGIN
          N := N + M;
          IF N > M THEN       -- ALWAYS TRUE.
               OVERFLOW_STACK;
          END IF;
          N := N - M;         -- TO PREVENT TAIL RECURSION OPTIMIZATION.
     END OVERFLOW_STACK;

BEGIN
     TEST ("CB1010D", "CHECK THAT STORAGE_ERROR IS RAISED WHEN " &
                      "STORAGE FOR THE EXECUTION OF A SUBPROGRAM " &
                      "IS INSUFFICIENT");

     -- CHECK HANDLING OF STORAGE_ERROR IN MAIN PROGRAM.

     BEGIN
          OVERFLOW_STACK;
          FAILED ("EXCEPTION NOT RAISED BY STACK OVERFLOW - 1");
     EXCEPTION
          WHEN STORAGE_ERROR =>
               IF N /= 1 THEN
                    FAILED ("VALUE OF VARIABLE N ALTERED - 1");
               END IF;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED BY STACK OVERFLOW - 1");
     END;

     -- CHECK HANDLING OF STORAGE_ERROR IN SUBPROGRAM.

     DECLARE

          PROCEDURE P IS
          BEGIN
               OVERFLOW_STACK;
               FAILED ("EXCEPTION NOT RAISED BY STACK OVERFLOW - 2");
          EXCEPTION
               WHEN STORAGE_ERROR =>
                    IF N /= 1 THEN
                         FAILED ("VALUE OF VARIABLE N ALTERED - 2");
                    END IF;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED BY STACK " &
                            "OVERFLOW - 2");
          END P;

     BEGIN

          N := IDENT_INT (1);
          P;

     END;

     RESULT;
END CB1010D;
