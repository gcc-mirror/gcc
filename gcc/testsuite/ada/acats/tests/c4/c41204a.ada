-- C41204A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF A SLICE'S DISCRETE
--   RANGE IS NOT NULL, AND ITS LOWER OR UPPER BOUND IS NOT A
--   POSSIBLE INDEX FOR THE NAMED ARRAY.

-- WKB 8/4/81
-- EDS 7/14/98    AVOID OPTIMIZATION

WITH REPORT;
USE REPORT;
PROCEDURE C41204A IS

BEGIN
     TEST ("C41204A", "ILLEGAL UPPER OR LOWER BOUNDS FOR A " &
                      "SLICE RAISES CONSTRAINT_ERROR");

     DECLARE

          TYPE T IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
          A : T (10..15) := (10,11,12,13,14,15);
          B : T (-20..30);

     BEGIN

          BEGIN
               B (IDENT_INT(9)..12) := A (IDENT_INT(9)..12);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 1" &
                       INTEGER'IMAGE(B(10)));
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS           => FAILED ("WRONG EXCEPTION - 1");
          END;

          BEGIN
               B (IDENT_INT(-12)..14) := A (IDENT_INT(-12)..14);
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 2" &
                       INTEGER'IMAGE(B(10)));
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS           => FAILED ("WRONG EXCEPTION - 2");
          END;

          BEGIN
               B (11..IDENT_INT(16)) := A (11..IDENT_INT(16));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 3" &
                       INTEGER'IMAGE(B(15)));
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS           => FAILED ("WRONG EXCEPTION - 3");
          END;

          BEGIN
               B (17..20) := A (IDENT_INT(17)..IDENT_INT(20));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 4" &
                       INTEGER'IMAGE(B(17)));
          EXCEPTION
               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS           => FAILED ("WRONG EXCEPTION - 4");
          END;
     END;

     RESULT;
END C41204A;
