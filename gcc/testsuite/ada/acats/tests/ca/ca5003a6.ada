-- CA5003A6M.ADA

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

-- SEPARATE FILES ARE:
--   CA5003A0  A LIBRARY PACKAGE.
--   CA5003A1  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A2  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A3  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A4  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A5  A LIBRARY PACKAGE SPECIFICATION.
--   CA5003A6M THE MAIN PROCEDURE.

-- PACKAGE A5 MUST BE ELABORATED AFTER A2, A3, AND A4.
-- PACKAGE A3 MUST BE ELABORATED AFTER A2.
-- PACKAGE A4 MUST BE ELABORATED AFTER A2.

-- WKB 7/22/81
-- JBG 10/6/83

WITH REPORT, CA5003A0;
USE REPORT, CA5003A0;
WITH CA5003A1, CA5003A5;
PROCEDURE CA5003A6M IS

BEGIN

     TEST ("CA5003A", "CHECK THAT ELABORATION ORDER IS CONSISTENT " &
                      "WITH PARTIAL ORDERING REQUIREMENTS");

     COMMENT ("ACTUAL ELABORATION ORDER WAS " & ORDER);

     IF ORDER /= "12345" AND
        ORDER /= "12435" AND
        ORDER /= "21345" AND
        ORDER /= "21435" AND
        ORDER /= "23145" AND
        ORDER /= "24135" AND
        ORDER /= "23415" AND
        ORDER /= "24315" AND
        ORDER /= "23451" AND
        ORDER /= "24351" THEN
          FAILED ("ILLEGAL ELABORATION ORDER");
     END IF;

     RESULT;
END CA5003A6M;
