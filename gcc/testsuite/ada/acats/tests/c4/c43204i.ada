-- C43204I.ADA

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
--     CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE CAN APPEAR AS THE
--     EXPRESSION IN AN ASSIGNMENT STATEMENT, AND THAT THE BOUNDS OF
--     THE AGGREGATE ARE DETERMINED CORRECTLY.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43204I IS

     TYPE ARR11 IS ARRAY (INTEGER RANGE -3 .. 3) OF INTEGER;
     TYPE ARR12 IS ARRAY (IDENT_INT(-3) .. IDENT_INT(3)) OF INTEGER;
     TYPE ARR13 IS ARRAY (IDENT_INT(1) .. IDENT_INT(-1)) OF INTEGER;
     TYPE ARR21 IS ARRAY (INTEGER RANGE -1 .. 1,
                          INTEGER RANGE -1 .. 1) OF INTEGER;
     TYPE ARR22 IS ARRAY (IDENT_INT(-1) .. IDENT_INT(1),
                          IDENT_INT(-1) .. IDENT_INT(1)) OF INTEGER;
     TYPE ARR23 IS ARRAY (INTEGER RANGE -1 .. 1,
                          IDENT_INT(-1) .. IDENT_INT(1)) OF INTEGER;
     TYPE ARR24 IS ARRAY (IDENT_INT(1) .. IDENT_INT(-1),
                          IDENT_INT(-1) .. IDENT_INT(1)) OF INTEGER;

     VA11 : ARR11;
     VA12 : ARR12;
     VA13 : ARR13;
     VA21 : ARR21;
     VA22 : ARR22;
     VA23 : ARR23;
     VA24 : ARR24;

BEGIN
     TEST ("C43204I", "CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE " &
                      "CAN APPEAR AS THE EXPRESSION IN AN ASSIGNMENT " &
                      "STATEMENT, AND THAT THE BOUNDS OF THE " &
                      "AGGREGATE ARE DETERMINED CORRECTLY");

     VA11 := (1,1, OTHERS => IDENT_INT(2));
     VA12 := (OTHERS => IDENT_INT(2));
     VA13 := (OTHERS => IDENT_INT(2));
     VA21 := ((1,1,1), OTHERS => (-1..1 => IDENT_INT(2)));
     VA22 := (-1 => (1,1,1), 0..1 => (OTHERS => IDENT_INT(2)));
     VA23 := (OTHERS => (OTHERS => IDENT_INT(2)));
     VA24 := (OTHERS => (OTHERS => IDENT_INT(2)));

     IF VA11 /= (1, 1, 2, 2, 2, 2, 2) THEN
          FAILED("INCORRECT VALUE OF VA11");
     END IF;

     IF VA12 /= (2, 2, 2, 2, 2, 2, 2) THEN
          FAILED("INCORRECT VALUE OF VA12");
     END IF;

     IF VA13'LENGTH /= 0 THEN
          FAILED("INCORRECT VALUE OF VA13");
     END IF;

     IF VA21 /= ((1,1,1), (2,2,2), (2,2,2)) THEN
          FAILED("INCORRECT VALUE OF VA21");
     END IF;

     IF VA22 /= ((1,1,1), (2,2,2), (2,2,2)) THEN
          FAILED("INCORRECT VALUE OF VA22");
     END IF;

     IF VA23 /= ((2,2,2), (2,2,2), (2,2,2)) THEN
          FAILED("INCORRECT VALUE OF VA23");
     END IF;

     IF VA24'LENGTH /= 0 OR VA24'LENGTH(2) /= 3 THEN
          FAILED("INCORRECT VALUE OF VA24");
     END IF;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ("UNEXPECTED CONSTRAINT_ERROR OR OTHER EXCEPTION " &
                  "RAISED");

          RESULT;
END C43204I;
