-- C45411A.ADA

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
--     PREDEFINED INTEGER OPERANDS.

-- HISTORY:
--     JET 01/25/88  CREATED ORIGINAL TEST.
--     PWN 10/27/95  REMOVED OUT OF RANGE STATIC VALUE CHECKS.

WITH REPORT; USE REPORT;

PROCEDURE C45411A IS

     TYPE DT IS NEW INTEGER RANGE -3..3;
     I1 : INTEGER := 1;
     D1 : DT := 1;

BEGIN
     TEST ("C45411A", "CHECK THAT UNARY ""+"" AND ""-"" YIELD " &
                      "CORRECT RESULTS FOR PREDEFINED INTEGER " &
                      "OPERANDS");

     FOR I IN (1-2)..INTEGER(1) LOOP
          IF "-"(RIGHT => I1) /= IDENT_INT(I) THEN
               FAILED ("INCORRECT RESULT FOR ""-"" -" &
                       INTEGER'IMAGE(I+2));
          END IF;

          IF +I1 /= IDENT_INT(I1) THEN
               FAILED ("INCORRECT RESULT FOR ""+"" -" &
                       INTEGER'IMAGE(I+2));
          END IF;
          I1 := I1 - 1;
     END LOOP;

     FOR I IN (1-2)..INTEGER(1) LOOP
          IF -I /= IDENT_INT(0)-I THEN
               FAILED ("INCORRECT RESULT FOR ""-"" -" &
                       INTEGER'IMAGE(I+5));
          END IF;

          IF "+"(RIGHT => IDENT_INT(I)) /= I THEN
               FAILED ("INCORRECT RESULT FOR ""+"" -" &
                       INTEGER'IMAGE(I+5));
          END IF;
     END LOOP;

     IF -1 /= IDENT_INT(1)-2 THEN
          FAILED ("INCORRECT RESULT FOR ""-"" - 7");
     END IF;

     IF "-"(RIGHT => 0) /= IDENT_INT(0) THEN
          FAILED ("INCORRECT RESULT FOR ""-"" - 8");
     END IF;

     IF "-"(RIGHT => "-"(RIGHT => 1)) /= IDENT_INT(1) THEN
          FAILED ("INCORRECT RESULT FOR ""-"" - 9");
     END IF;

     IF "+"(RIGHT => 1) /= IDENT_INT(2)-1 THEN
          FAILED ("INCORRECT RESULT FOR ""+"" - 7");
     END IF;

     IF +0 /= IDENT_INT(0) THEN
          FAILED ("INCORRECT RESULT FOR ""+"" - 8");
     END IF;

     IF +(-1) /= IDENT_INT(1)-2 THEN
          FAILED ("INCORRECT RESULT FOR ""+"" - 9");
     END IF;

     FOR I IN (1-2)..INTEGER(1) LOOP
          IF "-"(RIGHT => D1) /= DT(IDENT_INT(I)) THEN
               FAILED ("INCORRECT RESULT FOR ""-"" -" &
                       INTEGER'IMAGE(I+11));
          END IF;

          IF +D1 /= DT(IDENT_INT(INTEGER(D1))) THEN
               FAILED ("INCORRECT RESULT FOR ""+"" -" &
                       INTEGER'IMAGE(I+11));
          END IF;
          D1 := D1 - 1;
     END LOOP;

     IF INTEGER'LAST + INTEGER'FIRST = 0 THEN
          IF IDENT_INT(-INTEGER'LAST) /= INTEGER'FIRST THEN
               FAILED ("-INTEGER'LAST IS NOT EQUAL TO INTEGER'FIRST");
          END IF;
     ELSE
          IF IDENT_INT(-INTEGER'LAST) /= INTEGER'FIRST+1 THEN
               FAILED ("-INTEGER'LAST IS NOT EQUAL TO INTEGER'FIRST+1");
          END IF;
     END IF;

     RESULT;

END C45411A;
