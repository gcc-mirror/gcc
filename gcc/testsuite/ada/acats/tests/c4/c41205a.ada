-- C41205A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE NAME PART OF A
--   SLICE DENOTES AN ACCESS OBJECT WHOSE VALUE IS NULL, AND
--   ALSO IF THE NAME IS A FUNCTION CALL DELIVERING NULL.

-- WKB 8/6/81
-- SPS 10/26/82
-- EDS 07/14/98    AVOID OPTIMIZATION

WITH REPORT;
USE REPORT;
PROCEDURE C41205A IS

BEGIN
     TEST ("C41205A", "CONSTRAINT_ERROR WHEN THE NAME PART OF A " &
                      "SLICE DENOTES A NULL ACCESS OBJECT OR A " &
                      "FUNCTION CALL DELIVERING NULL");

     DECLARE

          TYPE T IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
          SUBTYPE T1 IS T (1..5);
          TYPE A1 IS ACCESS T1;
          B : A1 := NEW T1' (1,2,3,4,5);
          I : T (2..3);

     BEGIN

          IF EQUAL (3,3) THEN
               B := NULL;
          END IF;

          I := B(2..3);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 1 " & INTEGER'IMAGE(I(2)));

     EXCEPTION

          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS           => FAILED ("WRONG EXCEPTION - 1");

     END;

     DECLARE

          TYPE T IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
          SUBTYPE T2 IS T (1..5);
          TYPE A2 IS ACCESS T2;
          I : T (2..5);

          FUNCTION F RETURN A2 IS
          BEGIN
               IF EQUAL (3,3) THEN
                    RETURN NULL;
               END IF;
               RETURN NEW T2' (1,2,3,4,5);
          END F;

     BEGIN

          I := F(2..5);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 2 " & INTEGER'IMAGE(I(2)));

     EXCEPTION

          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS           => FAILED ("WRONG EXCEPTION - 2");

     END;

     RESULT;
END C41205A;
