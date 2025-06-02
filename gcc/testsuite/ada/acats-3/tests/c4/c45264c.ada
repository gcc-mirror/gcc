-- C45264C.ADA

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
-- CHECK THAT COMPARING ARRAYS OF DIFFERENT LENGTHS DOES NOT RAISE AN
-- EXCEPTION.

-- TBN  7/21/86

WITH REPORT; USE REPORT;
PROCEDURE C45264C IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 10;
     TYPE ARRAY_TYPE_1 IS ARRAY (INT RANGE <>) OF INTEGER;
     TYPE ARRAY_TYPE_2 IS ARRAY (INT RANGE <>, INT RANGE <>) OF INTEGER;
     TYPE ARRAY_TYPE_3 IS ARRAY (INT RANGE <>, INT RANGE <>,
                                               INT RANGE <>) OF INTEGER;

     ARRAY_1 : ARRAY_TYPE_1 (1..5) := (1..5 => 1);
     ARRAY_2 : ARRAY_TYPE_1 (1..7) := (1..7 => 1);
     ARRAY_3 : ARRAY_TYPE_2 (1..5, 1..4) := (1..5 => (1..4 => 1));
     ARRAY_4 : ARRAY_TYPE_2 (1..2, 1..3) := (1..2 => (1..3 => 1));
     ARRAY_5 : ARRAY_TYPE_3 (1..2, 1..3, 1..2) := (1..2 => (1..3 =>
                                                  (1..2 => 2)));
     ARRAY_6 : ARRAY_TYPE_3 (1..1, 1..2, 1..3) := (1..1 => (1..2 =>
                                                  (1..3 => 2)));
     ARRAY_7 : ARRAY_TYPE_2 (1..5, 1..4) := (1..5 => (1..4 => 3));
     ARRAY_8 : ARRAY_TYPE_2 (1..5, 1..3) := (1..5 => (1..3 => 3));
     ARRAY_9 : ARRAY_TYPE_2 (1..3, 1..2) := (1..3 => (1..2 => 4));
     ARRAY_10 : ARRAY_TYPE_2 (1..2, 1..2) := (1..2 => (1..2 => 4));

BEGIN
     TEST ("C45264C", "CHECK THAT COMPARING ARRAYS OF DIFFERENT " &
                      "LENGTHS DOES NOT RAISE AN EXCEPTION");

     BEGIN     -- (A)
          IF "=" (ARRAY_1 (1..INTEGER'FIRST), ARRAY_2) THEN
               FAILED ("INCORRECT RESULTS FROM COMPARING ONE " &
                       "DIMENSIONAL ARRAYS - 1");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED EVALUATING - 1");
     END;     -- (A)

     BEGIN     -- (B)
          IF ARRAY_1 /= ARRAY_2 THEN
               NULL;
          ELSE
               FAILED ("INCORRECT RESULTS FROM COMPARING ONE " &
                       "DIMENSIONAL ARRAYS - 2");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED EVALUATING - 2");
     END;     -- (B)

     BEGIN     -- (C)
          IF ARRAY_3 = ARRAY_4 THEN
               FAILED ("INCORRECT RESULTS FROM COMPARING MULTI-" &
                       "DIMENSIONAL ARRAYS - 3");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED EVALUATING - 3");
     END;     -- (C)

     BEGIN     -- (D)
          IF "/=" (ARRAY_3, ARRAY_4) THEN
               NULL;
          ELSE
               FAILED ("INCORRECT RESULTS FROM COMPARING MULT-" &
                       "DIMENSIONAL ARRAYS - 4");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - 4");
     END;     -- (D)

     BEGIN     -- (E)
          IF "=" (ARRAY_5, ARRAY_6) THEN
               FAILED ("INCORRECT RESULTS FROM COMPARING MULTI-" &
                       "DIMENSIONAL ARRAYS - 5");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED EVALUATING - 5");
     END;     -- (E)

     BEGIN     -- (F)
          IF ARRAY_6 /= ARRAY_5 THEN
               NULL;
          ELSE
               FAILED ("INCORRECT RESULTS FROM COMPARING MULT-" &
                       "DIMENSIONAL ARRAYS - 6");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED - 6");
     END;     -- (F)

     BEGIN     -- (G)
          IF ARRAY_7 = ARRAY_8 THEN
               FAILED ("INCORRECT RESULTS FROM COMPARING MULTI-" &
                       "DIMENSIONAL ARRAYS - 7");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED EVALUATING - 7");
     END;     -- (G)

     BEGIN     -- (H)
          IF ARRAY_9 /= ARRAY_10 THEN
               NULL;
          ELSE
               FAILED ("INCORRECT RESULTS FROM COMPARING MULTI-" &
                       "DIMENSIONAL ARRAYS - 8");
          END IF;

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED EVALUATING - 8");
     END;     -- (H)

     RESULT;
END C45264C;
