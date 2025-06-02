--  C55B04A.ADA

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
-- CHECK THAT A LOOP IS NOT ENTERED IF THE LOWER BOUND OF THE DISCRETE
--   RANGE IS GREATER THAN THE UPPER BOUND, WHETHER REVERSE IS PRESENT
--   OR NOT.

-- CHECK THAT LOOP BOUNDS ARE EVALUATED ONLY ONCE, UPON ENTRY TO 
--   THE LOOP.

-- DAS  01/12/81
-- SPS 3/2/83
-- JBG 8/21/83

WITH REPORT;
PROCEDURE C55B04A IS

     USE REPORT;

     C10 : CONSTANT INTEGER := 10;
     I10 : INTEGER;

BEGIN
     TEST ( "C55B04A", "CHECK OPERATION OF A FOR LOOP OVER A NULL " & 
                       "DISCRETE RANGE" );

     -- NOTE: EXIT STATEMENTS ARE INCLUDED TO AID IN RECOVERY FROM
     --   TEST FAILURE.

     -- SUBTESTS INVOLVING STATIC BOUNDS:

     FOR I IN 10..1 LOOP
          FAILED ( "LOOPING OVER NULL RANGE 10..1" );
          EXIT;
     END LOOP;

     FOR I IN REVERSE INTEGER RANGE -1..-10 LOOP
          FAILED ( "LOOPING OVER NULL RANGE -1..-10" );
          EXIT;
     END LOOP;

     FOR I IN (C10 + 3)..(-3 * C10 + 27) LOOP     -- 13..-3
          FAILED ("LOOPING OVER NULL RANGE (C10 + 3)..(-3 * C10 + 27)");
          EXIT;
     END LOOP;


     -- SUBTESTS INVOLVING DYNAMIC BOUNDS:

     I10 := IDENT_INT(10);

     FOR I IN REVERSE I10..(I10-1) LOOP           -- 10..9
          FAILED ( "LOOPING OVER NULL RANGE I10..(I10-1)");
          EXIT;
     END LOOP;


     FOR I IN (C10 - I10)..(I10 - 11) LOOP        -- 0..-1
          FAILED ( "LOOPING OVER NULL RANGE (C10 - I10)..(I10 - 11)" );
          EXIT;
     END LOOP;


     -- SUBTEST OF BOUNDS EVALUTION ONLY AT ENTRY:

     FOR I IN 1..I10 LOOP
          I10 := I10 - 1;
     END LOOP;
     IF (I10 /= 0) THEN
          FAILED ( "LOOP BOUNDS NOT FIXED AT LOOP ENTRY" );
     END IF;

     RESULT;

END C55B04A;
