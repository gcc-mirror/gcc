-- C41320A.ADA

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
--    CHECK THAT IMPLICITLY DECLARED ENUMERATION LITERALS, CHARACTER
--    LITERALS, AND THE RELATIONAL OPERATORS CAN BE SELECTED FROM
--    OUTSIDE THE PACKAGE USING AN EXPANDED NAME, FOR ENUMERATION TYPES.

-- HISTORY:
--    TBN 07/15/86  CREATED ORIGINAL TEST.
--    JET 08/04/87  ADDED TEST FOR OVERLOADED VARIABLES.

WITH REPORT; USE REPORT;
PROCEDURE C41320A IS

     PACKAGE P IS
          TYPE FLAG IS (RED, WHITE, BLUE);
          TYPE ROMAN_DIGITS IS ('I', 'V', 'X', 'C', 'M');
          TYPE TRAFFIC_LIGHT IS (RED, YELLOW, GREEN);
          TYPE HEX IS ('A', 'B', 'C', 'D', 'E', 'F');
          FLAG_COLOR_1 : FLAG := RED;
          FLAG_COLOR_2 : FLAG := WHITE;
          TRAFFIC_LIGHT_COLOR_1 : FLAG := RED;
          HEX_3 : HEX := 'C';
          ROMAN_1 : ROMAN_DIGITS := 'I';
     END P;

     USA_FLAG_1 : P.FLAG := P.RED;
     USA_FLAG_3 : P.FLAG := P.BLUE;
     HEX_CHAR_3 : P.HEX := P.'C';
     ROMAN_DIGITS_4 : P.ROMAN_DIGITS := P.'C';
     TRAFFIC_LIGHT_1 : P.TRAFFIC_LIGHT := P.RED;

BEGIN
     TEST ("C41320A", "CHECK THAT IMPLICITLY DECLARED ENUMERATION " &
                      "LITERALS, CHARACTER LITERALS, AND THE " &
                      "RELATIONAL OPERATORS CAN BE SELECTED FROM " &
                      "OUTSIDE THE PACKAGE USING AN EXPANDED NAME " &
                      "FOR ENUMERATION TYPES");

     IF P."/=" (USA_FLAG_1, P.FLAG_COLOR_1) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."=" (USA_FLAG_3, P.FLAG_COLOR_2) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     IF P."<" (HEX_CHAR_3, P.HEX_3) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 3");
     END IF;

     IF P.">" (P.ROMAN_1, ROMAN_DIGITS_4) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 4");
     END IF;

     IF P.">=" (TRAFFIC_LIGHT_1, P.TRAFFIC_LIGHT'PRED (P.GREEN)) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 5");
     END IF;

     FOR J IN P.FLAG'(P.WHITE) .. P.FLAG'(P.WHITE) LOOP
          IF P."<=" (P.FLAG'SUCC (P.WHITE), J) THEN
               FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 6");
          END IF;
     END LOOP;

     IF P.">=" (P.RED, P.GREEN) THEN
          FAILED ("INCORRECT RESULT FROM OVERLOADED VARIABLE NAME - 1");
     END IF;

     IF P."<=" (P.BLUE, P.RED) THEN
          FAILED ("INCORRECT RESULT FROM OVERLOADED VARIABLE NAME - 2");
     END IF;

     RESULT;
END C41320A;
