-- C43204F.ADA

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
--     CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE CAN APPEAR AS A
--     CONSTRAINED FORMAL PARAMETER OF A SUBPROGRAM AND THAT THE BOUNDS
--     OF THE AGGREGATE ARE DETERMINED CORRECTLY.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C43204F IS

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

     PROCEDURE PROC (PA11 : ARR11 := (1,1,1,1,1,1,
                                      OTHERS => IDENT_INT(2));
                     PA12 : ARR12 := (OTHERS => IDENT_INT(2));
                     PA13 : ARR13 := (OTHERS => IDENT_INT(2));
                     PA21 : ARR21 := ((1,1,1), (1,1,1),
                                      (1, OTHERS => IDENT_INT(2)));
                     PA22 : ARR22 := ((1,1,1), (1,1,1),
                                      (OTHERS => IDENT_INT(2)));
                     PA23 : ARR23 := ((1,1,1), (1,1,1), (1,1,1),
                                      OTHERS => (OTHERS =>
                                      IDENT_INT(2)));
                     PA24 : ARR24 := (OTHERS => (OTHERS =>
                                      IDENT_INT(2)))) IS
     BEGIN
          IF PA11 /= (1, 1, 1, 1, 1, 1, 2) THEN
               FAILED("INCORRECT VALUE OF PA11");
          END IF;

          IF PA12 /= (2, 2, 2, 2, 2, 2, 2) THEN
               FAILED("INCORRECT VALUE OF PA12");
          END IF;

          IF PA13'LENGTH /= 0 THEN
               FAILED("INCORRECT VALUE OF PA13");
          END IF;

          IF PA21 /= ((1,1,1), (1,1,1), (1,2,2)) THEN
               FAILED("INCORRECT VALUE OF PA21");
          END IF;

          IF PA22 /= ((1,1,1), (1,1,1), (2,2,2)) THEN
               FAILED("INCORRECT VALUE OF PA22");
          END IF;

          IF PA23 /= ((1,1,1), (1,1,1), (1,1,1)) THEN
               FAILED("INCORRECT VALUE OF PA23");
          END IF;

          IF PA24'LENGTH /= 0 OR PA24'LENGTH(2) /= 3 THEN
               FAILED("INCORRECT VALUE OF PA24");
          END IF;
     END PROC;

BEGIN
     TEST ("C43204F", "CHECK THAT AN AGGREGATE WITH AN OTHERS CLAUSE " &
                      "CAN APPEAR AS A CONSTRAINED FORMAL PARAMETER " &
                      "OF A SUBPROGRAM AND THAT THE BOUNDS OF THE " &
                      "AGGREGATE ARE DETERMINED CORRECTLY");

     PROC;

     RESULT;

EXCEPTION
     WHEN OTHERS =>
          FAILED ("UNEXPECTED CONSTRAINT_ERROR OR OTHER EXCEPTION " &
                  "RAISED");

          RESULT;
END C43204F;
