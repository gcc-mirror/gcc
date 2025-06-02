-- C41107A.ADA

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
-- CHECK THAT FOR AN ARRAY HAVING BOTH POSITIVE AND NEGATIVE
--   INDEX VALUES, THE PROPER COMPONENT IS SELECTED - A.
-- CHECK THAT FOR AN ARRAY INDEXED WITH AN ENUMERATION TYPE,
--   APPROPRIATE COMPONENTS CAN BE SELECTED - B.
-- CHECK THAT SUBSCRIPT EXPRESSIONS CAN BE OF COMPLEXITY GREATER
--   THAN VARIABLE + - CONSTANT - C.
-- CHECK THAT MULTIPLY DIMENSIONED ARRAYS ARE PROPERLY INDEXED - D.

-- WKB 7/29/81
-- JBG 8/21/83

WITH REPORT;
USE REPORT;
PROCEDURE C41107A IS

     TYPE T1 IS ARRAY (INTEGER RANGE -2..2) OF INTEGER;
     A : T1 := (1,2,3,4,5);

     TYPE COLOR IS (RED,ORANGE,YELLOW,GREEN,BLUE);
     TYPE T2 IS ARRAY (COLOR RANGE RED..BLUE) OF INTEGER;
     B : T2 := (5,4,3,2,1);

     C : STRING (1..7) := "ABCDEFG";

     TYPE T4 IS ARRAY (1..4,1..3) OF INTEGER;
     D : T4 := (1 => (1,2,3), 2 => (4,5,6), 3 => (7,8,9),
                4 => (0,-1,-2));

     V1 : INTEGER := IDENT_INT (1);
     V2 : INTEGER := IDENT_INT (2);
     V3 : INTEGER := IDENT_INT (3);

     PROCEDURE P1 (X : IN INTEGER; Y : IN OUT INTEGER;
                   Z : OUT INTEGER; W : STRING) IS
     BEGIN
          IF X /= 1 THEN
               FAILED ("WRONG VALUE FOR IN PARAMETER - " & W);
          END IF;
          IF Y /= 4 THEN
               FAILED ("WRONG VALUE FOR IN OUT PARAMETER - " & W);
          END IF;
          Y := 11;
          Z := 12;
     END P1;

     PROCEDURE P2 (X : IN CHARACTER; Y : IN OUT CHARACTER;
                   Z : OUT CHARACTER) IS
     BEGIN
          IF X /= 'D' THEN
               FAILED ("WRONG VALUE FOR IN PARAMETER - C");
          END IF;
          IF Y /= 'F' THEN
               FAILED ("WRONG VALUE FOR IN OUT PARAMETER - C");
          END IF;
          Y := 'Y';
          Z := 'Z';
     END P2;

BEGIN
     TEST ("C41107A", "CHECK THAT THE PROPER COMPONENT IS SELECTED " &
                      "FOR ARRAYS WITH POS AND NEG INDICES, " &  
                      "ENUMERATION INDICES, COMPLEX SUBSCRIPT " &
                      "EXPRESSIONS, AND MULTIPLE DIMENSIONS");

     IF A(IDENT_INT(1)) /= 4 THEN 
          FAILED ("WRONG VALUE FOR EXPRESSION - A");
     END IF;
     A(IDENT_INT(-2)) := 10;
     IF A /= (10,2,3,4,5) THEN
          FAILED ("WRONG TARGET FOR ASSIGNMENT - A");
     END IF;
     A := (2,1,0,3,4);
     P1 (A(-1), A(2), A(-2), "A");
     IF A /= (12,1,0,3,11) THEN
          FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - A");
     END IF;

     IF B(GREEN) /= 2 THEN
          FAILED ("WRONG VALUE FOR EXPRESSION - B");
     END IF;
     B(YELLOW) := 10;
     IF B /= (5,4,10,2,1) THEN
          FAILED ("WRONG TARGET FOR ASSIGNMENT - B");
     END IF;
     B := (1,4,2,3,5);
     P1 (B(RED), B(ORANGE), B(BLUE), "B");
     IF B /= (1,11,2,3,12) THEN
          FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - B");
     END IF;

     IF C(3..6)(3**2 / 3 * (2-1) - 6 / 3 + 2) /= 'C' THEN
          FAILED ("WRONG VALUE FOR EXPRESSION - C");
     END IF;
     C(3..6)(V3**2 / V1 * (V3-V2) + IDENT_INT(4) - V3 * V2 - V1) := 'W';
     IF C /= "ABCDEWG" THEN
          FAILED ("WRONG TARGET FOR ASSIGNMENT - C");
     END IF;
     C := "ABCDEFG";
     P2 (C(3..6)(V3+V1), C(3..6)(V3*V2), C(3..6)((V1+V2)*V1));
     IF C /= "ABZDEYG" THEN
          FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - C");
     END IF;

     IF D(IDENT_INT(1),IDENT_INT(3)) /= 3 THEN
          FAILED ("WRONG VALUE FOR EXPRESSION - D");
     END IF;
     D(IDENT_INT(4),IDENT_INT(2)) := 10;
     IF D /= ((1,2,3),(4,5,6),(7,8,9),(0,10,-2)) THEN
          FAILED ("WRONG TARGET FOR ASSIGNMENT - D");
     END IF;
     D := (1 => (0,2,3), 2 => (4,5,6), 3 => (7,8,9), 4 => (1,-1,-2));
     P1 (D(4,1), D(2,1), D(3,2), "D");
     IF D /= ((0,2,3),(11,5,6),(7,12,9),(1,-1,-2)) THEN
          FAILED ("WRONG TARGET FOR (IN) OUT PARAMETER - D");
     END IF;

     RESULT;
END C41107A;
