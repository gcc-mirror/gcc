-- CD1009Z.ADA

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
--     CHECK THAT A RECORD REPRESENTATION CLAUSE MAY BE GIVEN IN THE
--     PRIVATE PART OF A PACKAGE FOR A LIMITED-PRIVATE TYPE, WHOSE
--     FULL TYPE DECLARATION IS A RECORD TYPE, DECLARED IN THE VISIBLE
--     PART OF THE SAME PACKAGE.

-- HISTORY:
--     VCL 10/09/87  CREATED ORIGINAL TEST.
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP', CORRECTED
--                    CHECKS FOR FAILURE.

WITH SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CD1009Z IS
BEGIN
     TEST ("CD1009Z", "A RECORD REPRESENTATION CLAUSE MAY BE GIVEN " &
                      "IN THE PRIVATE PART OF A PACKAGE FOR A " &
                      "LIMITED PRIVATE TYPE, WHOSE FULL TYPE " &
                      "DECLARATION IS A RECORD TYPE DECLARED IN THE " &
                      "VISIBLE PART OF THE SAME PACKAGE");
     DECLARE
          PACKAGE PACK IS
               UNITS_PER_INTEGER : CONSTANT :=
                    (INTEGER'SIZE + SYSTEM.STORAGE_UNIT - 1) /
                    SYSTEM.STORAGE_UNIT;

               TYPE CHECK_TYPE_1 IS LIMITED PRIVATE;

               PROCEDURE P;
          PRIVATE
               TYPE CHECK_TYPE_1 IS
                    RECORD
                         I1 : INTEGER RANGE 0 .. 255;
                         B1 : BOOLEAN;
                         B2 : BOOLEAN;
                         I2 : INTEGER RANGE 0 .. 15;
                    END RECORD;
               FOR CHECK_TYPE_1 USE
                    RECORD
                         I1 AT 0 * UNITS_PER_INTEGER
                              RANGE 0 .. INTEGER'SIZE - 1;
                         B1 AT 1 * UNITS_PER_INTEGER
                              RANGE 0 .. BOOLEAN'SIZE - 1;
                         B2 AT 2 * UNITS_PER_INTEGER
                              RANGE 0 .. BOOLEAN'SIZE - 1;
                         I2 AT 3 * UNITS_PER_INTEGER
                              RANGE 0 .. INTEGER'SIZE - 1;
                    END RECORD;
          END PACK;

          PACKAGE BODY PACK IS
               PROCEDURE P IS
                    R1 : CHECK_TYPE_1;
               BEGIN
                    IF R1.I1'FIRST_BIT /= 0 OR
                              R1.I1'LAST_BIT /= INTEGER'SIZE - 1 OR
                              R1.I1'POSITION /= 0 THEN
                         FAILED ("INCORRECT REPRESENTATION FOR R1.I1");
                    END IF;

                    IF R1.B1'FIRST_BIT /= 0 OR
                              R1.B1'LAST_BIT /= BOOLEAN'SIZE - 1 OR
                              R1.B1'POSITION /= 1 * UNITS_PER_INTEGER
                                   THEN
                         FAILED ("INCORRECT REPRESENTATION FOR R1.B1");
                    END IF;

                    IF R1.B2'FIRST_BIT /= 0 OR
                              R1.B2'LAST_BIT /= BOOLEAN'SIZE - 1 OR
                              R1.B2'POSITION /= 2 * UNITS_PER_INTEGER
                                   THEN
                         FAILED ("INCORRECT REPRESENTATION FOR R1.B2");
                    END IF;

                    IF R1.I2'FIRST_BIT /= 0 OR
                              R1.I2'LAST_BIT /= INTEGER'SIZE - 1 OR
                              R1.I2'POSITION /= 3 * UNITS_PER_INTEGER
                                   THEN
                         FAILED ("INCORRECT REPRESENTATION FOR R1.I2");
                    END IF;
               END P;
          END PACK;

          USE PACK;

     BEGIN
          P;
     END;

     RESULT;
END CD1009Z;
