-- C55B05A.ADA

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
-- CHECK THAT LOOPS WITH BOUNDS INTEGER'LAST OR
-- INTEGER'FIRST DO NOT RAISE INVALID EXCEPTIONS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- DAT 3/26/81
-- SPS 3/2/83
-- MRM 03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;

PROCEDURE C55B05A IS
BEGIN
     TEST ("C55B05A", "LOOPS WITH INTEGER'FIRST AND 'LAST AS BOUNDS");

     DECLARE

          COUNT : INTEGER := 0;

          PROCEDURE C IS
          BEGIN
               COUNT := COUNT + 1;
          END C;

     BEGIN
          FOR I IN INTEGER'LAST .. INTEGER'FIRST LOOP
               FAILED ("WRONG NULL RANGE LOOP EXECUTION");
               EXIT;
          END LOOP;
          FOR I IN INTEGER'FIRST .. INTEGER'FIRST LOOP
               C;
          END LOOP;
          FOR I IN INTEGER'FIRST .. INTEGER'FIRST + 2 LOOP
               C; C;
          END LOOP;
          FOR I IN INTEGER'FIRST + 1 .. INTEGER'FIRST LOOP
               FAILED ("NULL RANGE ERROR 2");
               EXIT;
          END LOOP;
          FOR I IN INTEGER'FIRST .. INTEGER'LAST LOOP
               C;
               EXIT;
          END LOOP;
          FOR I IN INTEGER LOOP
               C;
               EXIT;
          END LOOP;
          FOR I IN INTEGER'LAST - 2 .. INTEGER'LAST LOOP
               C; C; C;
          END LOOP;
          FOR I IN INTEGER'LAST - 2 .. INTEGER'LAST - 1 LOOP
               C;
          END LOOP;
          FOR I IN 0 .. INTEGER'FIRST LOOP
               FAILED ("NULL LOOP ERROR 3");
               EXIT;
          END LOOP;
          FOR I IN -1 .. INTEGER'FIRST LOOP
               FAILED ("NULL LOOP ERROR 4");
               EXIT;
          END LOOP;
          FOR I IN -3 .. IDENT_INT(0) LOOP
               FOR J IN INTEGER'FIRST .. INTEGER'FIRST - I LOOP
                    C; C; C; C;
               END LOOP;
               FOR J IN INTEGER'FIRST - I .. INTEGER'FIRST + 3 - I LOOP
                    C; C; C; C;
               END LOOP;
               FOR J IN INTEGER'LAST - 3 .. INTEGER'LAST + I LOOP
                    C; C; C; C;
               END LOOP;
               FOR J IN INTEGER'LAST + I .. INTEGER'LAST LOOP
                    C; C; C; C;
               END LOOP;
          END LOOP;

          FOR I IN REVERSE INTEGER'LAST .. INTEGER'FIRST LOOP
               FAILED ("REVERSE WRONG NULL RANGE LOOP EXECUTION");
               EXIT;
          END LOOP;
          FOR I IN REVERSE INTEGER'FIRST .. INTEGER'FIRST LOOP
               C;
          END LOOP;
          FOR I IN REVERSE INTEGER'FIRST .. INTEGER'FIRST + 2 LOOP
               C; C;
          END LOOP;
          FOR I IN REVERSE INTEGER'FIRST + 1 .. INTEGER'FIRST LOOP
               FAILED ("NULL RANGE ERROR 8");
               EXIT;
          END LOOP;
          FOR I IN REVERSE INTEGER'FIRST .. INTEGER'LAST LOOP
               C;
               EXIT;
          END LOOP;
          FOR I IN REVERSE INTEGER LOOP
               C;
               EXIT;
          END LOOP;
          FOR I IN REVERSE INTEGER'LAST - 2 .. INTEGER'LAST LOOP
               C; C; C;
          END LOOP;
          FOR I IN REVERSE INTEGER'LAST - 2 .. INTEGER'LAST - 1 LOOP
               C;
          END LOOP;
          FOR I IN REVERSE 0 .. INTEGER'FIRST LOOP
               FAILED ("NULL LOOP ERROR 9");
               EXIT;
          END LOOP;
          FOR I IN REVERSE -1 .. INTEGER'FIRST LOOP
               FAILED ("NULL LOOP ERROR 7");
               EXIT;
          END LOOP;
          FOR I IN REVERSE -3 .. IDENT_INT(0) LOOP
               FOR J IN REVERSE INTEGER'FIRST .. INTEGER'FIRST - I LOOP
                    C; C; C; C;
               END LOOP;
               FOR J IN REVERSE INTEGER'FIRST - I
                    .. INTEGER'FIRST + 3 - I
               LOOP
                    C; C; C; C;
               END LOOP;
               FOR J IN REVERSE INTEGER'LAST - 3 .. INTEGER'LAST + I
               LOOP
                    C; C; C; C;
               END LOOP;
               FOR J IN REVERSE INTEGER'LAST + I .. INTEGER'LAST LOOP
                    C; C; C; C;
               END LOOP;
          END LOOP;

          IF COUNT /= 408 THEN
               FAILED ("WRONG LOOP EXECUTION COUNT");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ("CONSTRAINT_ERROR RAISED INCORRECTLY");
          WHEN OTHERS =>
               FAILED ("UNKNOWN EXCEPTION RAISED INCORRECTLY");
     END;

     RESULT;
END C55B05A;
