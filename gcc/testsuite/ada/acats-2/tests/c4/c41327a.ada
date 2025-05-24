-- C41327A.ADA

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
-- CHECK THAT IMPLICITLY DECLARED EQUALITY AND INEQUALITY OPERATORS
-- MAY BE SELECTED FROM OUTSIDE A PACKAGE USING AN EXPANDED NAME, FOR
-- A PRIVATE TYPE.

-- TBN  7/18/86

WITH REPORT; USE REPORT;
PROCEDURE C41327A IS

     PACKAGE P IS
          TYPE KEY IS PRIVATE;
          TYPE CHAR IS PRIVATE;
          FUNCTION INIT_KEY (X : NATURAL) RETURN KEY;
          FUNCTION INIT_CHAR (X : CHARACTER) RETURN CHAR;
     PRIVATE
          TYPE KEY IS NEW NATURAL;
          TYPE CHAR IS NEW CHARACTER;
     END P;

     VAR_KEY_1 : P.KEY;
     VAR_KEY_2 : P.KEY;
     VAR_CHAR_1 : P.CHAR;
     VAR_CHAR_2 : P.CHAR;

     PACKAGE BODY P IS

          FUNCTION INIT_KEY (X : NATURAL) RETURN KEY IS
          BEGIN
               RETURN (KEY (X));
          END INIT_KEY;

          FUNCTION INIT_CHAR (X : CHARACTER) RETURN CHAR IS
          BEGIN
               RETURN (CHAR (X));
          END INIT_CHAR;

     BEGIN
          NULL;
     END P;

BEGIN
     TEST ("C41327A", "CHECK THAT IMPLICITLY DECLARED EQUALITY AND " &
                      "INEQUALITY OPERATORS MAY BE SELECTED FROM " &
                      "OUTSIDE A PACKAGE USING AN EXPANDED NAME, " &
                      "FOR A PRIVATE TYPE");

     VAR_KEY_1 := P.INIT_KEY (1);
     VAR_KEY_2 := P.INIT_KEY (2);
     VAR_CHAR_1 := P.INIT_CHAR ('A');
     VAR_CHAR_2 := P.INIT_CHAR ('A');
     IF P."=" (VAR_KEY_1, VAR_KEY_2) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 1");
     END IF;

     IF P."/=" (VAR_CHAR_1, VAR_CHAR_2) THEN
          FAILED ("INCORRECT RESULTS FROM EXPANDED NAME - 2");
     END IF;

     RESULT;
END C41327A;
