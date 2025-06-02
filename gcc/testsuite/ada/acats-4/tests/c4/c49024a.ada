-- C49024A.ADA

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
-- CHECK THAT A FUNCTION CALL CAN APPEAR IN A STATIC EXPRESSION IF THE
-- FUNCTION NAME DENOTES A PREDEFINED OPERATOR AND HAS THE FORM OF AN
-- OPERATOR SYMBOL OR AN EXPANDED NAME WHOSE SELECTOR IS AN OPERATOR
-- SYMBOL.

-- L.BROWN  10/02/86

WITH REPORT; USE REPORT;
PROCEDURE  C49024A  IS

     PACKAGE P IS
          TYPE TY IS NEW INTEGER;
     END P;

     CON1 : CONSTANT P.TY := 3;
     CON2 : CONSTANT P.TY := 4;
     TYPE INT1 IS RANGE 1 .. P."+"(CON1,CON2);
     CON3 : CONSTANT := 5;
     CON4 : CONSTANT := 7;
     TYPE FLT IS DIGITS "-"(CON4,CON3);
     TYPE FIX1 IS DELTA 1.0 RANGE 0.0 .. 25.0;
     CON5 : CONSTANT := 3.0;
     CON6 : CONSTANT := 6.0;
     TYPE FIX2 IS DELTA 1.0 RANGE 0.0 .. "/"(CON6,CON5);
     TYPE ENUM IS (RED,BLUE,GREEN,BLACK);
     CON7 : CONSTANT BOOLEAN := TRUE;
     CON8 : CONSTANT ENUM := BLUE;
     CAS_INT1 : CONSTANT := 10;
     CAS_INT2 : CONSTANT := 2;
     OBJ1 : INTEGER := 10;
     CAS_BOL : BOOLEAN := TRUE;
     CON9 : CONSTANT ENUM := BLACK;
     CON10 : CONSTANT FIX1 := 2.0;
     CON11 : CONSTANT FIX1 := 10.0;
     TYPE FIX3 IS DELTA "+"(CON10) RANGE 0.0 .. 20.0;
     TYPE INT2 IS RANGE 0 .. "ABS"("-"(CON4));
     CON12 : CONSTANT CHARACTER := 'D';
     CON13 : CONSTANT CHARACTER := 'B';
     CON14 : CONSTANT BOOLEAN := FALSE;
     CON15 : CONSTANT := 10;

BEGIN

     TEST("C49024A","A FUNCTION CALL CAN BE IN A STATIC EXPRESSION "&
                    "IF THE FUNCTION NAME DENOTES A PREDEFINED "&
                    "OPERATOR AND HAS THE FORM OF AN OPERATOR SYMBOL");

     CASE CAS_BOL IS
          WHEN ("NOT"(CON7)) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 1");
          WHEN ("/="(CON8,CON9)) =>
               OBJ1 := 2;
     END CASE;
     CAS_BOL := TRUE;

     CASE CAS_BOL IS
          WHEN ("*"(CON3,CON4) = CAS_INT1) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 2");
          WHEN ("ABS"(CON15) = CAS_INT1) =>
               OBJ1 := 3;
     END CASE;
     CAS_BOL := TRUE;

     CASE CAS_BOL IS
          WHEN ("<"(CON11,CON10)) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 3");
          WHEN ("<="(CON13,CON12)) =>
               OBJ1 := 4;
     END CASE;
     CAS_BOL := TRUE;

     CASE CAS_BOL IS
          WHEN ("REM"(CON4,CON3) = CAS_INT2) =>
               OBJ1 := 5;
          WHEN ("**"(CON3,CON4) = CAS_INT2) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 4");
     END CASE;

     CASE CAS_BOL IS
          WHEN (P.">"(CON1,CON2)) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 5");
          WHEN ("OR"(CON7,CON14)) =>
               OBJ1 := 6;
     END CASE;
     CAS_BOL := TRUE;

     CASE CAS_BOL IS
          WHEN ("MOD"(CON4,CON3) = CAS_INT2) =>
               OBJ1 := 7;
          WHEN ("ABS"(CON4) = CAS_INT2) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 6");
     END CASE;

     CASE CAS_BOL IS
          WHEN ("AND"(CON7,CON14)) =>
               FAILED("INCORRECT VALUE RETURNED FOR STATIC "&
                      "OPERATORS 7");
          WHEN (">="(CON12,CON13)) =>
               OBJ1 := 9;
     END CASE;

     RESULT;

END C49024A;
