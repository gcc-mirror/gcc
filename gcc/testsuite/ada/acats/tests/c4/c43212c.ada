-- C43212C.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL SUBAGGREGATES FOR
-- A PARTICULAR DIMENSION DO NOT HAVE THE SAME BOUNDS.
-- ADDITIONAL CASES FOR THE THIRD DIMENSION AND FOR THE NULL ARRAYS.

-- PK  02/21/84
-- EG  05/30/84

WITH REPORT;
USE REPORT;

PROCEDURE C43212C IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 3;

BEGIN

     TEST("C43212C","CHECK THAT CONSTRAINT_ERROR IS RAISED IF ALL " &
                    "SUBAGGREGATES FOR A PARTICULAR DIMENSION DO "  &
                    "NOT HAVE THE SAME BOUNDS");

     DECLARE
          TYPE A3 IS ARRAY(INT RANGE <>, INT RANGE <>, INT RANGE <>)
                         OF INTEGER;
     BEGIN
          IF A3'(((IDENT_INT(1) .. IDENT_INT(2) => IDENT_INT(1)),
                      (1 .. IDENT_INT(2) => IDENT_INT(1))),
                     ((IDENT_INT(2) .. IDENT_INT(3) => IDENT_INT(1)),
                      (IDENT_INT(2) .. IDENT_INT(3) => IDENT_INT(1)))) 
            =
             A3'(((IDENT_INT(1) .. IDENT_INT(2) => IDENT_INT(1)),
                      (1 .. IDENT_INT(2) => IDENT_INT(1))),
                     ((IDENT_INT(2) .. IDENT_INT(3) => IDENT_INT(1)),
                      (IDENT_INT(2) .. IDENT_INT(3) => IDENT_INT(1)))) 
          THEN
                FAILED ("A3 - EXCEPTION NOT RAISED, ARRAYS EQUAL");
          END IF;
          FAILED ("A3 - EXCEPTION NOT RAISED, ARRAYS NOT EQUAL");

     EXCEPTION

          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => 
               FAILED ("A3 - WRONG EXCEPTION RAISED");

     END;

     DECLARE

          TYPE B3 IS ARRAY(INT RANGE <>, INT RANGE <>, INT RANGE <>)
                         OF INTEGER;

     BEGIN

          IF B3'(((IDENT_INT(2) .. IDENT_INT(1) => IDENT_INT(1)),
                      (2 .. IDENT_INT(1) => IDENT_INT(1))),
                     ((IDENT_INT(3) .. IDENT_INT(1) => IDENT_INT(1)),
                      (IDENT_INT(3) .. IDENT_INT(1) => IDENT_INT(1)))) 
            =
             B3'(((IDENT_INT(2) .. IDENT_INT(1) => IDENT_INT(1)),
                      (2 .. IDENT_INT(1) => IDENT_INT(1))),
                     ((IDENT_INT(3) .. IDENT_INT(1) => IDENT_INT(1)),
                      (IDENT_INT(3) .. IDENT_INT(1) => IDENT_INT(1)))) 
          THEN
                FAILED ("B3 - EXCEPTION NOT RAISED, ARRAYS EQUAL");
          END IF;
          FAILED ("B3 - EXCEPTION NOT RAISED, ARRAYS NOT EQUAL");

     EXCEPTION

          WHEN CONSTRAINT_ERROR => NULL;
          WHEN OTHERS => 
                FAILED ("B3 - WRONG EXCEPTION RAISED");

     END;

     RESULT;

END C43212C;
