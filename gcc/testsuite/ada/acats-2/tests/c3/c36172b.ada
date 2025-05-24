-- C36172B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A MULTIDIMENSIONAL INDEX
-- CONSTRAINT IF ONE OF THE RANGES IS A NULL RANGE AND THE OTHER IS A
-- NON-NULL RANGE WITH A BOUND THAT LIES OUTSIDE THE INDEX SUBTYPE.

-- CHECK THAT NO EXCEPTION IS RAISED IF ALL DISCRETE RANGES ARE NULL.

-- JBG 6/5/85
-- EDS 7/16/98     AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C36172B IS
     SUBTYPE INT_10 IS INTEGER RANGE 1..10;
     TYPE ARR2 IS ARRAY (INT_10 RANGE <>, INT_10 RANGE <>) OF INTEGER;
BEGIN
     TEST ("C36172B", "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
                      "NON-NULL DIMENSION OF A NULL MULTIDIMENSIONAL " &
                      "INDEX CONSTRAINT IF A BOUND LIES OUTSIDE THE " &
                      "INDEX SUBTYPE");

     BEGIN
          DECLARE
               V : ARR2 (6..4, 9..11);
          BEGIN
               FAILED ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
                       "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
                       "THE INDEX SUBTYPE (13) " & INTEGER'IMAGE(V'FIRST));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 13");
     END;

     BEGIN
          DECLARE
               V : ARR2 (0..3, 8..7);
          BEGIN
               FAILED ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
                       "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
                       "THE INDEX SUBTYPE (14) " & INTEGER'IMAGE(V'FIRST));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 14");
     END;

     BEGIN
          DECLARE
               V : ARR2 (6..4, IDENT_INT(0)..2);
          BEGIN
               FAILED ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
                       "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
                       "THE INDEX SUBTYPE (15) " & INTEGER'IMAGE(V'FIRST));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 15");
     END;

     BEGIN
          DECLARE
               V : ARR2 (9..IDENT_INT(11), 6..4);
          BEGIN
               FAILED ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
                       "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
                       "THE INDEX SUBTYPE (16) " & INTEGER'IMAGE(V'FIRST));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 16");
     END;

     BEGIN
          DECLARE
               V : ARR2 (6..IDENT_INT(4), 9..IDENT_INT(11));
          BEGIN
               FAILED ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
                       "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
                       "THE INDEX SUBTYPE (17) " & INTEGER'IMAGE(V'FIRST));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 17");
     END;

     BEGIN
          DECLARE
               V : ARR2 (IDENT_INT(-1)..2, IDENT_INT(6)..4);
          BEGIN
               FAILED ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
                       "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
                       "THE INDEX SUBTYPE (18) " & INTEGER'IMAGE(V'FIRST));
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR => 
               NULL;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 18");
     END;

     BEGIN
          DECLARE
               V : ARR2 (6..-1, 11..9);
          BEGIN
               IF NOT EQUAL (V'FIRST, V'FIRST) THEN
                    FAILED ("IMPOSSIBLE");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED FOR NULL CONSTRAINT - 19");
     END;

     BEGIN
          DECLARE
               V : ARR2 (IDENT_INT(11)..9, 6..IDENT_INT(0));
          BEGIN
               IF NOT EQUAL (V'FIRST, V'FIRST) THEN
                    FAILED ("IMPOSSIBLE");
               END IF;
          END;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED FOR NULL CONSTRAINT - 20");
     END;

     RESULT;
END C36172B; 
