-- C41328A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED DERIVED SUBPROGRAMS CAN BE SELECTED
-- FROM OUTSIDE A PACKAGE USING AN EXPANDED NAME, FOR A DERIVED TYPE.

-- TBN  7/21/86

WITH REPORT; USE REPORT;
PROCEDURE C41328A IS

     PACKAGE P IS
          PACKAGE Q IS
               TYPE PAIR IS ARRAY (1..2) OF INTEGER;
               FUNCTION INIT (INT : INTEGER) RETURN PAIR;
               PROCEDURE SWAP (TWO : IN OUT PAIR);
          END Q;
          TYPE COUPLE IS NEW Q.PAIR;
     END P;

     VAR_1 : P.COUPLE;
     VAR_2 : P.COUPLE;

     PACKAGE BODY P IS

          PACKAGE BODY Q IS

               FUNCTION INIT (INT : INTEGER) RETURN PAIR IS
                    A : PAIR;
               BEGIN
                    A (1) := INT;
                    A (2) := INT + 1;
                    RETURN (A);
               END INIT;

               PROCEDURE SWAP (TWO : IN OUT PAIR) IS
                    TEMP : INTEGER;
               BEGIN
                    TEMP := TWO (1);
                    TWO (1) := TWO (2);
                    TWO (2) := TEMP;
               END SWAP;

          BEGIN
               NULL;
          END Q;

     BEGIN
          NULL;
     END P;

BEGIN
     TEST ("C41328A", "CHECK THAT IMPLICITLY DECLARED DERIVED " &
                      "SUBPROGRAMS CAN BE SELECTED FROM OUTSIDE A " &
                      "PACKAGE USING AN EXPANDED NAME, FOR A DERIVED " &
                      "TYPE");

     VAR_1 := P.INIT (IDENT_INT(1));
     IF P."/=" (VAR_1, P.COUPLE'(1 => 1, 2 => 2)) THEN
          FAILED ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 1");
     END IF;

     VAR_2 := P.INIT (IDENT_INT(2));
     IF P."=" (VAR_2, P.COUPLE'(1 => 1, 2 => 2)) THEN
          FAILED ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 2");
     END IF;

     P.SWAP (VAR_1);
     IF P."=" (VAR_1, P.COUPLE'(1 => 1, 2 => 2)) THEN
          FAILED ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 3");
     END IF;

     P.SWAP (VAR_2);
     IF P."/=" (VAR_2, P.COUPLE'(1 => 3, 2 => 2)) THEN
          FAILED ("INCORRECT RESULTS FROM DERIVED SUBPROGRAM - 4");
     END IF;

     RESULT;
END C41328A;
