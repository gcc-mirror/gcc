-- C66002G.ADA

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

--     (G) THE RESULT TYPE OF TWO FUNCTION DECLARATIONS IS DIFFERENT.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C66002G IS

     USE REPORT;

BEGIN
     TEST ("C66002G", "SUBPROGRAM OVERLOADING WITH " &
           "MINIMAL DIFFERENCES ALLOWED");

     --------------------------------------------------

     -- THE RESULT TYPES OF TWO FUNCTION
     -- DECLARATIONS ARE DIFFERENT.

     DECLARE
          I : INTEGER;
          B : BOOLEAN;
          S : STRING (1..2) := "12";

          FUNCTION F RETURN INTEGER IS
          BEGIN
               S(1) := 'A';
               RETURN IDENT_INT (0); -- THIS VALUE IS IRRELEVENT.
          END F;

          FUNCTION F RETURN BOOLEAN IS
          BEGIN
               S(2) := 'B';
               RETURN IDENT_BOOL (TRUE); -- THIS VALUE IS IRRELEVANT.
          END F;

     BEGIN
          I := F;
          B := F;

          IF S /= "AB" THEN
               FAILED ("FUNCTIONS DIFFERING ONLY IN " &
                       "BASE TYPE OF RETURNED VALUE " &
                       "CAUSED CONFUSION");
          END IF;
     END;

     --------------------------------------------------

     RESULT;

END C66002G;
