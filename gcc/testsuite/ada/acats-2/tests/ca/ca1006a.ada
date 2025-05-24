-- CA1006A.ADA

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
-- CHECK THAT A LIBRARY UNIT AND ITS SUBUNITS CAN BE
-- SUBMITTED TOGETHER FOR COMPILATION.

-- JRK 5/14/81

WITH REPORT;
USE REPORT;

PROCEDURE CA1006A IS

     I : INTEGER := IDENT_INT (0);

     PACKAGE CALL_TEST IS
     END CALL_TEST;

     PACKAGE BODY CALL_TEST IS
     BEGIN
          TEST ("CA1006A", "A LIBRARY UNIT AND ITS SUBUNITS " &
                "SUBMITTED TOGETHER");
     END CALL_TEST;

     FUNCTION F (I : INTEGER) RETURN INTEGER IS SEPARATE;

     PACKAGE PKG IS
          I : INTEGER := IDENT_INT (0);
          PROCEDURE P (I : IN OUT INTEGER);
     END PKG;

     PACKAGE BODY PKG IS SEPARATE;

     PROCEDURE P (I : IN OUT INTEGER) IS SEPARATE;

BEGIN

     IF PKG.I /= 10 THEN
          FAILED ("PACKAGE BODY STATEMENTS NOT EXECUTED");
     END IF;

     IF F(IDENT_INT(5)) /= -5 THEN
          FAILED ("FUNCTION NOT ELABORATED/EXECUTED");
     END IF;

     PKG.P (I);
     IF I /= 3 THEN
          FAILED ("PACKAGED PROCEDURE NOT ELABORATED/EXECUTED");
     END IF;

     I := IDENT_INT (-20);
     P (I);
     IF I /= -24 THEN
          FAILED ("PROCEDURE NOT ELABORATED/EXECUTED");
     END IF;

     RESULT;
END CA1006A;


SEPARATE (CA1006A)
FUNCTION F (I : INTEGER) RETURN INTEGER IS
BEGIN
     RETURN -I;
END F;


SEPARATE (CA1006A)
PACKAGE BODY PKG IS

     PROCEDURE P (I : IN OUT INTEGER) IS
     BEGIN
          I := I + 3;
     END P;

BEGIN
     I := I + 10;
END PKG;


SEPARATE (CA1006A)
PROCEDURE P (I : IN OUT INTEGER) IS
BEGIN
     I := I - 4;
END P;
