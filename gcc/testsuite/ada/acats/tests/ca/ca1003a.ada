-- CA1003A.ADA

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
-- CHECK THAT MORE THAN ONE COMPLETELY INDEPENDENT COMPILATION
-- UNIT CAN BE SUBMITTED IN A SINGLE FILE.

-- JRK 5/13/81
-- JBG 8/25/83

PROCEDURE CA1003A_P (I : IN OUT INTEGER) IS
BEGIN
     I := I + 1;
END CA1003A_P;


PACKAGE CA1003A_PKG IS
     I : INTEGER := 0;
END CA1003A_PKG;


FUNCTION CA1003A_F (I : INTEGER) RETURN INTEGER IS
BEGIN
     RETURN -I;
END CA1003A_F;


WITH REPORT, CA1003A_P, CA1003A_PKG, CA1003A_F;
USE REPORT;

PROCEDURE CA1003A IS

     I : INTEGER := IDENT_INT (0);

BEGIN
     TEST ("CA1003A", "INDEPENDENT UNITS IN A SINGLE FILE");

     CA1003A_P (I);
     IF I /= 1 THEN
          FAILED ("INDEPENDENT PROCEDURE NOT INVOKED");
     END IF;

     CA1003A_PKG.I := CA1003A_PKG.I + IDENT_INT(10);
     IF CA1003A_PKG.I /= 10 THEN
          FAILED ("INDEPENDENT PACKAGE VARIABLE ACCESSED INCORRECTLY");
     END IF;

     IF CA1003A_F(IDENT_INT(5)) /= -5 THEN
          FAILED ("INDEPENDENT FUNCTION NOT INVOKED");
     END IF;

     RESULT;
END CA1003A;
