-- CA1014A0M.ADA

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
-- CHECK THAT A SUBUNIT CAN BE SUBMITTED FOR COMPILATION
-- SEPARATELY FROM ITS PARENT UNIT.

-- SEPARATE FILES ARE:
--   CA1014A0M THE MAIN PROCEDURE.
--   CA1014A1  A SUBUNIT PROCEDURE BODY.
--   CA1014A2  A SUBUNIT PACKAGE BODY.
--   CA1014A3  A SUBUNIT FUNCTION BODY.

-- JRK 5/20/81

WITH REPORT;
USE REPORT;

PROCEDURE CA1014A0M IS

     I : INTEGER := 0;

     PACKAGE CALL_TEST IS
     END CALL_TEST;

     PACKAGE BODY CALL_TEST IS
     BEGIN
          TEST ("CA1014A", "SUBUNITS SUBMITTED FOR COMPILATION " &
                "SEPARATELY FROM PARENT UNIT");
     END CALL_TEST;

     PROCEDURE CA1014A1 (I : IN OUT INTEGER) IS SEPARATE;

     PACKAGE CA1014A2 IS
          I : INTEGER := 10;
          PROCEDURE P (I : IN OUT INTEGER);
     END CA1014A2;

     PACKAGE BODY CA1014A2 IS SEPARATE;

     FUNCTION CA1014A3 (I : INTEGER) RETURN INTEGER IS SEPARATE;

BEGIN

     CA1014A1 (I);
     IF I /= 1 THEN
          FAILED ("SUBUNIT PROCEDURE NOT ELABORATED/EXECUTED");
     END IF;

     IF CA1014A2.I /= 15 THEN
          FAILED ("SUBUNIT PACKAGE BODY NOT ELABORATED/EXECUTED");
     END IF;

     I := 0;
     CA1014A2.P (I);
     IF I /= -20 THEN
          FAILED ("SUBUNIT PACKAGED PROCEDURE NOT ELABORATED/EXECUTED");
     END IF;

     IF CA1014A3(50) /= -50 THEN
          FAILED ("SUBUNIT FUNCTION NOT ELABORATED/EXECUTED");
     END IF;

     RESULT;
END CA1014A0M;
