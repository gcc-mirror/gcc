-- C66002F.ADA

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

--     (F) ONE SUBPROGRAM IS DECLARED IN AN OUTER DECLARATIVE PART,
--         THE OTHER IN AN INNER PART, AND ONE HAS ONE MORE PARAMETER
--         THAN THE OTHER; THE OMITTED PARAMETER HAS A DEFAULT VALUE.

-- CVP 5/4/81
-- JRK 5/8/81
-- NL 10/13/81

WITH REPORT;
PROCEDURE C66002F IS

     USE REPORT;

BEGIN
     TEST ("C66002F", "SUBPROGRAM OVERLOADING WITH " &
           "MINIMAL DIFFERENCES ALLOWED");

     --------------------------------------------------

     -- ONE SUBPROGRAM IS IN AN OUTER DECLARATIVE 
     -- PART, THE OTHER IN AN INNER PART, AND ONE
     -- HAS ONE MORE PARAMETER (WITH A DEFAULT
     -- VALUE) THAN THE OTHER.

     BF : 
     DECLARE
          S : STRING (1..3) := "123";

          PROCEDURE P (I1, I2, I3 : INTEGER := 1) IS
               C : CONSTANT STRING := "CXA";
          BEGIN
               S(I3) := C(I3);
          END P;

          PROCEDURE ENCLOSE IS
 
               PROCEDURE P (I1, I2 : INTEGER := 1) IS
               BEGIN
                    S(2) := 'B';
               END P;

          BEGIN -- ENCLOSE
               P (1, 2, 3);
               ENCLOSE.P (1, 2); -- NOTE THAT THESE CALLS
               BF.P (1, 2);      -- MUST BE DISAMBIGUATED.

               IF S /= "CBA" THEN
                    FAILED ("PROCEDURES IN ENCLOSING-" &
                            "ENCLOSED SCOPES DIFFERING " &
                            "ONLY IN EXISTENCE OF ONE " &
                            "DEFAULT-VALUED PARAMETER CAUSED " &
                            "CONFUSION");
               END IF;
          END ENCLOSE;

     BEGIN
          ENCLOSE;
     END BF;

     --------------------------------------------------

     RESULT;

END C66002F;
