-- C66002A.ADA

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
-- CHECK THAT OVERLOADED SUBPROGRAM DECLARATIONS
-- ARE PERMITTED IN WHICH THERE IS A MINIMAL
-- DIFFERENCE BETWEEN THE DECLARATIONS.

--     (A) ONE SUBPROGRAM IS A FUNCTION; THE OTHER IS A PROCEDURE.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81
-- SPS 11/2/82

WITH REPORT;
PROCEDURE C66002A IS

     USE REPORT;

BEGIN
     TEST ("C66002A", "SUBPROGRAM OVERLOADING WITH " &
           "MINIMAL DIFFERENCES ALLOWED");

     --------------------------------------------------

     -- ONE SUBPROGRAM IS A PROCEDURE; THE OTHER IS
     -- A FUNCTION. BOTH PARAMETERIZED AND PARAMETERLESS
     -- SUBPROGRAMS ARE TESTED.

     DECLARE
          I, J, K : INTEGER := 0;
          S : STRING (1..2) := "12";

          PROCEDURE P1 (I1, I2 : INTEGER) IS
          BEGIN
               S(1) := 'A';
          END P1;

          FUNCTION P1 (I1, I2 : INTEGER) RETURN INTEGER IS
          BEGIN
               S(2) := 'B';
               RETURN I1; -- RETURNED VALUE IS IRRELEVENT.
          END P1;

          PROCEDURE P2 IS
          BEGIN
               S(1) := 'C';
          END P2;

          FUNCTION P2 RETURN INTEGER IS
          BEGIN
               S(2) := 'D';
               RETURN I; -- RETURNED VALUE IS IRRELEVENT.
          END P2;

     BEGIN
          P1 (I, J);
          K := P1 (I, J);

          IF S /= "AB" THEN
               FAILED ("PARAMETERIZED OVERLOADED " &
                       "SUBPROGRAMS, ONE A PROCEDURE AND " &
                       "THE OTHER A FUNCTION, CAUSED " &
                       "CONFUSION");
          END IF;

          S := "12";
          P2;
          K := P2 ;

          IF S /= "CD" THEN 
               FAILED ("PARAMETERLESS OVERLOADED " &
                       "SUBPROGRAMS, ONE A PROCEDURE AND " &
                       "THE OTHER A FUNCTION, CAUSED " &
                       "CONFUSION");
          END IF;
     END;

     --------------------------------------------------

     RESULT;

END C66002A;
