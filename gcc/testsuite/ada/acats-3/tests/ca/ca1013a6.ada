-- CA1013A6M.ADA

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
-- CHECK THAT A GENERIC PACKAGE OR SUBPROGRAM INSTANTIATION
-- CAN BE SUBMITTED FOR SEPARATE COMPILATION.

-- SEPARATE FILES ARE:
--   CA1013A0  A LIBRARY GENERIC PACKAGE.
--   CA1013A1  A LIBRARY GENERIC PROCEDURE.
--   CA1013A2  A LIBRARY GENERIC FUNCTION.
--   CA1013A3  A LIBRARY GENERIC PACKAGE INSTANTIATION.
--   CA1013A4  A LIBRARY GENERIC PROCEDURE INSTANTIATION.
--   CA1013A5  A LIBRARY GENERIC FUNCTION INSTANTIATION.
--   CA1013A6M THE MAIN PROCEDURE.

-- WKB 7/20/81
-- SPS 11/5/82

WITH REPORT;
WITH CA1013A3, CA1013A4, CA1013A5;
USE REPORT;
PROCEDURE CA1013A6M IS

     J : INTEGER := 1;

BEGIN
     TEST ("CA1013A", "GENERIC INSTANTIATIONS SUBMITTED " &
                      "FOR SEPARATE COMPILATION");

     IF CA1013A3.I /= 1 THEN
          FAILED ("PACKAGE NOT ACCESSED");
     END IF;

     CA1013A4 (J);
     IF J /= 2 THEN
          FAILED ("PROCEDURE NOT INVOKED");
     END IF;

     IF CA1013A5 /= 2 THEN
          FAILED ("FUNCTION NOT INVOKED");
     END IF;

     RESULT;
END CA1013A6M;
