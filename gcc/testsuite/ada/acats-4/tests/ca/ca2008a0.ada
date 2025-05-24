-- CA2008A0M.ADA

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
-- CHECK THAT FOR AN OVERLOADED SUBPROGRAM, ONE OF THE
--   SUBPROGRAM BODIES CAN BE SPECIFIED WITH A BODY_STUB AND
--   COMPILED SEPARATELY.

-- SEPARATE FILES ARE:
--   CA2008A0M THE MAIN PROCEDURE.
--   CA2008A1  A SUBUNIT PROCEDURE BODY.
--   CA2008A2  A SUBUNIT FUNCTION BODY.

-- WKB 6/26/81
-- SPS 11/2/82

WITH REPORT;
USE REPORT;
PROCEDURE CA2008A0M IS

     I : INTEGER := 0;
     B : BOOLEAN := TRUE;

     PROCEDURE CA2008A1 (I : IN OUT INTEGER) IS
     BEGIN
          I := IDENT_INT (1);
     END CA2008A1;

     PROCEDURE CA2008A1 (B : IN OUT BOOLEAN) IS SEPARATE;

     FUNCTION CA2008A2 RETURN INTEGER IS SEPARATE;

     FUNCTION CA2008A2 RETURN BOOLEAN IS
     BEGIN
          RETURN IDENT_BOOL (FALSE);
     END CA2008A2;

BEGIN
     TEST ("CA2008A", "CHECK THAT AN OVERLOADED SUBPROGRAM " &
                      "CAN HAVE ONE OF ITS BODIES COMPILED SEPARATELY");

     CA2008A1 (I);
     IF I /= 1 THEN
          FAILED ("OVERLOADED PROCEDURE NOT INVOKED - 1");
     END IF;

     CA2008A1 (B);
     IF B THEN
          FAILED ("OVERLOADED PROCEDURE NOT INVOKED - 2");
     END IF;

     IF CA2008A2 /= 2 THEN
          FAILED ("OVERLOADED FUNCTION NOT INVOKED - 1");
     END IF;

     IF CA2008A2 THEN
          FAILED ("OVERLOADED FUNCTION NOT INVOKED - 2");
     END IF;

     RESULT;
END CA2008A0M;
