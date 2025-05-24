-- C32001A.ADA

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
-- CHECK THAT IN MULTIPLE OBJECT DECLARATIONS FOR SCALAR TYPES, THE 
-- SUBTYPE INDICATION AND THE INITIALIZATION EXPRESSIONS ARE EVALUATED
-- ONCE FOR EACH NAMED OBJECT THAT IS DECLARED AND THE SUBTYPE 
-- INDICATION IS EVALUATED FIRST.  ALSO, CHECK THAT THE EVALUATIONS 
-- YIELD THE SAME RESULT AS A SEQUENCE OF SINGLE OBJECT DECLARATIONS.

-- RJW 7/16/86

WITH REPORT; USE REPORT;

PROCEDURE C32001A IS

     BUMP : ARRAY (1 .. 8) OF INTEGER := (OTHERS => 0);

     FUNCTION F (I : INTEGER) RETURN INTEGER IS
     BEGIN
          BUMP (I) := BUMP (I) + 1;
          RETURN BUMP (I);
     END F;
          
BEGIN
     TEST ("C32001A", "CHECK THAT IN MULTIPLE OBJECT DECLARATION " & 
                      "FOR SCALAR TYPES, THE SUBTYPE INDICATION " & 
                      "AND THE INITIALIZATION EXPRESSIONS ARE " &
                      "EVALUATED ONCE FOR EACH NAMED OBJECT THAT " &
                      "IS DECLARED AND THE SUBTYPE INDICATION IS " &
                      "EVALUATED FIRST.  ALSO, CHECK THAT THE " &
                      "EVALUATIONS YIELD THE SAME RESULT AS A " &
                      "SEQUENCE OF SINGLE OBJECT DECLARATIONS" );
     
     DECLARE

          TYPE DAY IS (MON, TUES, WED, THURS, FRI);
          D1, D2   : DAY 
                     RANGE MON .. DAY'VAL (F (1)) :=
                     DAY'VAL (F (1) - 1);
          CD1, CD2 : CONSTANT DAY 
                     RANGE MON .. DAY'VAL (F (2)) :=
                     DAY'VAL (F (2) - 1);

          I1, I2   : INTEGER RANGE 0 .. F (3) :=
                     F (3) - 1;
          CI1, CI2 : CONSTANT INTEGER RANGE 0 .. F (4) 
                     := F (4) - 1;

          TYPE FLT IS DIGITS 3 RANGE -5.0 .. 5.0;
          FL1, FL2   : FLT RANGE 0.0 .. FLT (F (5)) :=
                       FLT (F (5) - 1);
          CFL1, CFL2 : CONSTANT FLT 
                       RANGE 0.0 .. FLT (F (6)) := 
                       FLT (F (6) - 1);

          TYPE FIX IS DELTA 1.0 RANGE -5.0 .. 5.0;
          FI1, FI2   : FIX RANGE 0.0 .. FIX (F (7)) :=
                       FIX (F (7) - 1);
          CFI1, CFI2 : CONSTANT FIX 
                       RANGE 0.0 .. FIX (F (8)) := 
                       FIX (F (8) - 1);

     BEGIN
          IF D1 /= TUES THEN 
               FAILED ( "D1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF D2 /= THURS THEN 
               FAILED ( "D2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CD1 /= TUES THEN 
               FAILED ( "CD1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CD2 /= THURS THEN 
               FAILED ( "CD2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF I1 /= 1 THEN 
               FAILED ( "I1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF I2 /= 3 THEN 
               FAILED ( "I2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CI1 /= 1 THEN 
               FAILED ( "CI1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CI2 /= 3 THEN 
               FAILED ( "CI2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF FL1 /= 1.0 THEN 
               FAILED ( "FL1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF FL2 /= 3.0 THEN 
               FAILED ( "FL2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CFL1 /= 1.0 THEN 
               FAILED ( "CFL1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CFL2 /= 3.0 THEN 
               FAILED ( "CFL2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF FI1 /= 1.0 THEN 
               FAILED ( "FI1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF FI2 /= 3.0 THEN 
               FAILED ( "FI2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CFI1 /= 1.0 THEN 
               FAILED ( "CFI1 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

          IF CFI2 /= 3.0 THEN 
               FAILED ( "CFI2 NOT INITIALIZED TO CORRECT VALUE" );
          END IF;

     END;

     RESULT;
END C32001A;
