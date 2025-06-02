-- CA5003B5M.ADA

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
-- CHECK THAT THE ELABORATION OF LIBRARY UNITS REQUIRED BY
--   A MAIN PROGRAM IS PERFORMED CONSISTENTLY WITH THE PARTIAL
--   ORDERING DEFINED BY THE COMPILATION ORDER RULES.
-- IN PARTICULAR, CHECK THAT A LIBRARY UNIT MENTIONED IN THE
--   WITH_CLAUSE OF A SUBUNIT IS ELABORATED PRIOR TO THE BODY OF 
--   THE ANCESTOR UNIT.

-- SEPARATE FILES ARE:
--   CA5003B0  A LIBRARY PACKAGE.
--   CA5003B1  A LIBRARY PACKAGE.
--   CA5003B2  A SUBUNIT PACKAGE BODY (_B1._B2).
--   CA5003B3  A LIBRARY PACKAGE DECLARATION.
--   CA5003B4  A SUBUNIT PACKAGE BODY (_B1._B2._B4).
--   CA5003B5M THE MAIN PROCEDURE.

-- LIBRARY PACKAGES MUST BE ELABORATED IN ORDER: _B0, _B3, _B1.
-- PARENT UNITS MUST BE ELABORATED BEFORE THEIR SUBUNITS.

-- WKB 7/22/81
-- JBG 10/6/83
-- BHS 8/02/84
-- JRK 9/20/84

WITH REPORT, CA5003B0;
USE REPORT, CA5003B0;
WITH CA5003B1;
PROCEDURE CA5003B5M IS

BEGIN
     TEST ("CA5003B", "CHECK THAT UNITS IN WITH_CLAUSES OF " &
                      "SUBUNITS ARE ELABORATED PRIOR TO THE " &
                      "BODY OF THE ANCESTOR UNIT");

     COMMENT ("ACTUAL ELABORATION ORDER WAS " & ORDER);

     IF ORDER /= "3124" THEN
          FAILED ("ILLEGAL ELABORATION ORDER");
     END IF;

     RESULT;
END CA5003B5M;
