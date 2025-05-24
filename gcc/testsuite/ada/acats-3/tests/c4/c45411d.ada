-- C45411D.ADA

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
--     CHECK THAT UNARY "+" AND "-" YIELD CORRECT RESULTS FOR
--     OPERANDS OF DERIVED INTEGER TYPES.

-- HISTORY:
--     JET 07/11/88  CREATED ORIGINAL TEST.
--     PWN 10/27/95  REMOVED OUT OF RANGE STATIC VALUE CHECKS.

WITH REPORT; USE REPORT;

PROCEDURE C45411D IS

     TYPE INT IS RANGE -100..100;

     TYPE DT1 IS NEW INTEGER;
     TYPE DT2 IS NEW INT;

     D1 : DT1 := 1;
     D2 : DT2 := 1;

     FUNCTION IDENT (A : DT1) RETURN DT1 IS
     BEGIN
          RETURN A * DT1(IDENT_INT(1));
     END IDENT;

     FUNCTION IDENT (A : DT2) RETURN DT2 IS
     BEGIN
          RETURN A * DT2(IDENT_INT(1));
     END IDENT;

BEGIN
     TEST ("C45411D", "CHECK THAT UNARY ""+"" AND ""-"" YIELD " &
                      "CORRECT RESULTS FOR OPERANDS OF DERIVED " &
                      "INTEGER TYPES");

     FOR I IN DT1'(1-2)..DT1'(1) LOOP
          IF "-"(RIGHT => D1) /= IDENT(I) THEN
               FAILED ("INCORRECT RESULT FOR ""-"" DT1 -" &
                       DT1'IMAGE(I+2));
          END IF;

          IF +D1 /= IDENT(D1) THEN
               FAILED ("INCORRECT RESULT FOR ""+"" DT1 -" &
                       DT1'IMAGE(I+2));
          END IF;
          D1 := D1 - 1;
     END LOOP;

     IF DT1'LAST + DT1'FIRST = 0 THEN
          IF IDENT(-DT1'LAST) /= DT1'FIRST THEN
               FAILED ("-DT1'LAST IS NOT EQUAL TO DT1'FIRST");
          END IF;
     ELSE
          IF IDENT(-DT1'LAST) /= DT1'FIRST+1 THEN
               FAILED ("-DT1'LAST IS NOT EQUAL TO DT1'FIRST+1");
          END IF;
     END IF;

     FOR I IN DT2'(1-2)..DT2'(1) LOOP
          IF -D2 /= IDENT(I) THEN
               FAILED ("INCORRECT RESULT FOR ""-"" DT2 -" &
                       DT2'IMAGE(I+2));
          END IF;

          IF "+"(RIGHT => D2) /= IDENT(D2) THEN
               FAILED ("INCORRECT RESULT FOR ""+"" DT2 -" &
                       DT2'IMAGE(I+2));
          END IF;
          D2 := D2 - 1;
     END LOOP;

     RESULT;

END C45411D;
