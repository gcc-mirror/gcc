-- C66002D.ADA

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

--     (D) THE BASE TYPE OF A PARAMETER IS DIFFERENT FROM THAT
--         OF THE CORRESPONDING ONE.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81

WITH REPORT;
PROCEDURE C66002D IS

     USE REPORT;

BEGIN
     TEST ("C66002D", "SUBPROGRAM OVERLOADING WITH " &
           "MINIMAL DIFFERENCES ALLOWED");

     --------------------------------------------------

     -- THE BASE TYPE OF ONE PARAMETER IS
     -- DIFFERENT FROM THAT OF THE CORRESPONDING
     -- ONE.

     DECLARE
          I, J, K : INTEGER := 0;
          B : BOOLEAN;
          S : STRING (1..2) := "12";

          PROCEDURE P (I1 : INTEGER; BI : OUT BOOLEAN;
                       I2 : IN OUT INTEGER) IS
          BEGIN
               S(1) := 'A';
               BI := TRUE; -- THIS VALUE IS IRRELEVENT.
          END P;

          PROCEDURE P (I1 : INTEGER; BI : OUT INTEGER;
               I2 : IN OUT INTEGER) IS
          BEGIN
               S(2) := 'B';
               BI := 0; -- THIS VALUE IS IRRELEVENT.
          END P;

     BEGIN
          P (I, B, K);
          P (I, J, K);

          IF S /= "AB" THEN
               FAILED ("PROCEDURES DIFFERING ONLY BY " &
                       "THE BASE TYPE OF A PARAMETER " &
                       "CAUSED CONFUSION");
          END IF;
     END;

     --------------------------------------------------

     RESULT;

END C66002D;
