-- C66002E.ADA

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

--     (E) ONE SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE
--         PART, THE OTHER IN AN INNER PART, AND THE PARAMETERS ARE 
--         ORDERED DIFFERENTLY.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81

WITH REPORT;
PROCEDURE C66002E IS

     USE REPORT;

BEGIN
     TEST ("C66002E", "SUBPROGRAM OVERLOADING WITH " &
           "MINIMAL DIFFERENCES ALLOWED");

     --------------------------------------------------

     -- ONE SUBPROGRAM IS DECLARED IN AN OUTER
     -- DECLARATIVE PART, THE OTHER IN AN INNER
     -- PART, AND THE PARAMETERS ARE ORDERED
     -- DIFFERENTLY.

     DECLARE
          S : STRING (1..2) := "12";

          PROCEDURE P (I1 : INTEGER; I2 : IN OUT INTEGER;
                       B1 : BOOLEAN) IS
          BEGIN
               S(1) := 'A';
          END P;

     BEGIN
          DECLARE
               I : INTEGER := 0;

               PROCEDURE P (B1 : BOOLEAN; I1 : INTEGER;
                            I2 : IN OUT INTEGER) IS
               BEGIN
                    S(2) := 'B';
               END P;

          BEGIN
               P (5, I, TRUE);
               P (TRUE, 5, I);
               -- NOTE THAT A CALL IN WHICH ALL ACTUAL PARAMETERS 
               -- ARE NAMED_ASSOCIATIONS IS AMBIGUOUS.

               IF S /= "AB" THEN
                    FAILED ("PROCEDURES IN " &
                            "ENCLOSING-ENCLOSED SCOPES " &
                            "DIFFERING ONLY IN PARAMETER " &
                            "TYPE ORDER CAUSED CONFUSION");
               END IF;
          END;
     END;

     --------------------------------------------------

     RESULT;

END C66002E;
