-- C49020A.ADA

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
-- CHECK THAT ENUMERATION LITERALS (INCLUDING CHARACTER LITERALS) CAN BE
-- USED IN STATIC EXPRESSIONS TOGETHER WITH RELATIONAL AND EQUALITY 
-- OPERATORS.

-- L.BROWN   09/30/86

WITH REPORT; USE REPORT;
PROCEDURE  C49020A  IS

     CAS_BOL : BOOLEAN := TRUE;
     OBJ1 : INTEGER := 4;
     TYPE ENUM IS (RED,GREEN,BLUE,OFF,ON,'A','B');

BEGIN
     TEST("C49020A","ENUMERATION LITERALS (INCLUDING CHARACTER "&
                    "LITERALS) TOGETHER WITH RELATIONAL OPERATORS "&
                    "CAN BE USED IN STATIC EXPRESSION");

     CASE CAS_BOL IS
          WHEN (RED <= BLUE) =>
               OBJ1 := 5;
          WHEN (BLUE = GREEN) =>
               FAILED("INCORRECT VALUE RETURNED BY ENUMERATION "&
                      "EXPRESSION 1");
     END CASE;

     CAS_BOL := TRUE;

     CASE CAS_BOL IS
          WHEN (GREEN >= ON) =>
               FAILED("INCORRECT VALUE RETURNED BY ENUMERATION "&
                      "EXPRESSION 2");
          WHEN (ENUM'('A') < ENUM'('B')) =>
               OBJ1 := 6;
     END CASE;

     CAS_BOL := TRUE;

     CASE CAS_BOL IS
          WHEN (BLUE > 'B') =>
               FAILED("INCORRECT VALUE RETURNED BY ENUMERATION "&
                      "EXPRESSION 3");
          WHEN (OFF /= 'A') =>
               OBJ1 := 7;
     END CASE;

     RESULT;

END C49020A;
