-- C49021A.ADA

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
-- CHECK THAT BOOLEAN LITERALS CAN BE USED IN STATIC EXPRESSIONS 
-- TOGETHER WITH THE LOGICAL OPERATORS, THE NOT OPERATOR, AND THE
-- RELATIONAL AND EQUALITY OPERATORS.

-- L.BROWN  09/25/86

WITH REPORT; USE REPORT;
PROCEDURE  C49021A  IS

     CAS_BOL : BOOLEAN := TRUE;
     X1 : CONSTANT := BOOLEAN'POS((TRUE AND FALSE)OR(TRUE AND TRUE));
     X2 : CONSTANT := BOOLEAN'POS((TRUE <= FALSE)AND(FALSE >= FALSE));

BEGIN
     TEST("C49021A","BOOLEAN LITERALS TOGETHER WITH CERTAIN OPERATORS,"&
                    "CAN BE USED IN STATIC EXPRESSIONS.");
     IF X1 /= 1  THEN
          FAILED("INCORRECT VALUE RETURNED BY BOOLEAN EXPRESSION 1");
     END IF;

     IF X2 /= 0  THEN
          FAILED("INCORRECT VALUE RETURNED BY BOOLEAN EXPRESSION 2");
     END IF;

     CASE CAS_BOL IS
          WHEN ((TRUE AND FALSE) XOR (TRUE XOR TRUE)) =>
               FAILED("INCORRECT VALUE RETURNED BY BOOLEAN " &
                      "EXPRESSION 2");
          WHEN OTHERS =>
               CAS_BOL := TRUE;
     END CASE;

     CASE CAS_BOL IS
          WHEN ((TRUE > FALSE) OR (FALSE <= TRUE)) =>
               CAS_BOL := TRUE;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED BY BOOLEAN " &
                      "EXPRESSION 3");
     END CASE;

     CASE CAS_BOL IS
          WHEN NOT((TRUE OR FALSE) = (FALSE AND TRUE)) =>
               CAS_BOL := TRUE;
          WHEN OTHERS =>
               FAILED("INCORRECT VALUE RETURNED BY BOOLEAN " &
                      "EXPRESSION 4");
     END CASE;

     CASE CAS_BOL IS
          WHEN (((TRUE = FALSE) OR (FALSE AND TRUE)) /= (TRUE < TRUE))=>
               FAILED("INCORRECT VALUE RETURNED BY BOOLEAN " &
                      "EXPRESSION 5");
          WHEN OTHERS =>
               CAS_BOL := TRUE;
     END CASE;

     RESULT;

END C49021A;
