-- C85017A.ADA

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
-- OBJECTIVE:
--     CHECK THAT RENAMING A PREDEFINED OPERATION WITH AN IDENTIFIER
--     AND THEN RENAMING THE IDENTIFIER AS AN OPERATOR SYMBOL ALLOWS THE
--     NEW NAME TO BE USED IN A STATIC EXPRESSION.

-- HISTORY:
--     JET 03/24/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85017A IS

     FUNCTION PLUS (L,R : INTEGER) RETURN INTEGER RENAMES "+";
     FUNCTION MINUS (L,R : INTEGER) RETURN INTEGER RENAMES "-";

     FUNCTION "-" (L,R : INTEGER) RETURN INTEGER RENAMES PLUS;
     FUNCTION "+" (L,R : INTEGER) RETURN INTEGER RENAMES MINUS;

     I1 : CONSTANT INTEGER := 10 + 10;
     I2 : CONSTANT INTEGER := 10 - 10;

     TYPE INT IS RANGE I1 .. I2;
BEGIN
     TEST("C85017A","CHECK THAT RENAMING A PREDEFINED OPERATION WITH " &
                    "AN IDENTIFIER AND THEN RENAMING THE IDENTIFIER " &
                    "AS AN OPERATOR SYMBOL ALLOWS THE NEW NAME TO BE " &
                    "USED IN A STATIC EXPRESSION");

     IF I1 /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF I1: " & INTEGER'IMAGE(I1));
     END IF;

     IF I2 /= IDENT_INT(20) THEN
          FAILED ("INCORRECT VALUE OF I2: " & INTEGER'IMAGE(I2));
     END IF;

     RESULT;
END C85017A;
