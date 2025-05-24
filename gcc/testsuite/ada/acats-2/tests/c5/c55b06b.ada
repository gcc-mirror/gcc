-- C55B06B.ADA

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
-- CHECK THAT LOOPS MAY BE SPECIFIED FOR DERIVED BOOLEAN AND
-- DERIVED DERIVED BOOLEAN.

-- DAT 3/26/81
-- SPS 3/2/83

WITH REPORT; USE REPORT;

PROCEDURE C55B06B IS

     TYPE E IS (FALSE, TRUE);
     TYPE B1 IS NEW BOOLEAN;
     TYPE B2 IS NEW B1;
     TYPE B3 IS NEW E;

     ONE : INTEGER := IDENT_INT (1);
     COUNT : INTEGER := 0;
     OLD_COUNT : INTEGER := 0;

     PROCEDURE Q IS
     BEGIN
          COUNT := COUNT + 1;
     END Q;

BEGIN
     TEST ("C55B06B", "LOOPS OVER DERIVED BOOLEAN");

     FOR I IN BOOLEAN LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 1");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN BOOLEAN RANGE FALSE .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 2");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN BOOLEAN'(FALSE) .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 3");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN E LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 4");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN E RANGE FALSE .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 5");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN FALSE .. E'(TRUE) LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 6");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B1 LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 7");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B1 RANGE FALSE .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 8");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN FALSE .. B1'(TRUE) LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 9");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B2 LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 10");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B2 RANGE FALSE .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 11");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B2'(FALSE) .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 12");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B3 LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 13");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN B3 RANGE FALSE .. TRUE LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 14");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     FOR I IN FALSE .. B3'(TRUE) LOOP
          Q;
     END LOOP;
     IF OLD_COUNT + IDENT_INT (2) /= COUNT THEN
          FAILED ("LOOP 15");
     ELSE
          OLD_COUNT := COUNT;
     END IF;

     RESULT;
 END C55B06B;
