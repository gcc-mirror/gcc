-- CA1004A.ADA

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
-- CHECK THAT A PACKAGE DECLARATION AND BODY CAN BE
-- SUBMITTED TOGETHER FOR COMPILATION.

-- JRK 5/12/81


PACKAGE CA1004A_PKG IS

     I : INTEGER := 0;

     PROCEDURE P (I : IN OUT INTEGER);

END CA1004A_PKG;


PACKAGE BODY CA1004A_PKG IS

     PROCEDURE P (I : IN OUT INTEGER) IS
     BEGIN
          I := I + 1;
     END P;

BEGIN

     I := 10;

END CA1004A_PKG;


WITH REPORT, CA1004A_PKG;
USE REPORT;

PROCEDURE CA1004A IS

     I : INTEGER := IDENT_INT (0);

BEGIN
     TEST ("CA1004A", "A PACKAGE DECLARATION AND BODY SUBMITTED " &
           "TOGETHER");

     CA1004A_PKG.I := CA1004A_PKG.I + IDENT_INT(5);
     IF CA1004A_PKG.I /= 15 THEN
          FAILED ("PACKAGED VARIABLE NOT ACCESSIBLE OR " &
                  "PACKAGE BODY NOT EXECUTED");
     END IF;

     CA1004A_PKG.P (I);
     IF I /= 1 THEN
          FAILED ("PACKAGED PROCEDURE NOT EXECUTED");
     END IF;

     RESULT;
END CA1004A;
