-- C83007A.ADA

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
--     CHECK THAT A FORMAL PARAMETER OF A SUBPROGRAM DECLARED BY A
--     RENAMING DECLARATION CAN HAVE THE SAME IDENTIFIER AS A
--     DECLARATION IN THE BODY OF THE RENAMED SUBPROGRAM.

-- HISTORY:
--     VCL  02/18/88  CREATED ORIGINAL TEST.


WITH REPORT;  USE REPORT;
PROCEDURE C83007A IS
BEGIN
     TEST ("C83007A", "A FORMAL PARAMETER OF A SUBPROGRAM DECLARED " &
                      "BY A RENAMING DECLARATION CAN HAVE THE SAME " &
                      "IDENTIFIER AS A DECLARATION IN THE BODY OF " &
                      "THE RENAMED SUBPROGRAM");
     DECLARE
          PROCEDURE P (ONE : INTEGER; TWO : FLOAT; THREE : STRING);

          PROCEDURE R (D1 : INTEGER;
                       D2 : FLOAT;
                       D3 : STRING)  RENAMES P;

          PROCEDURE P (ONE : INTEGER; TWO : FLOAT; THREE : STRING) IS
               TYPE D1 IS RANGE 1..10;
               I : D1 := D1(IDENT_INT (7));

               D2 : FLOAT;

               FUNCTION D3 RETURN STRING IS
               BEGIN
                    RETURN "D3";
               END D3;

               FUNCTION IDENT_FLOAT (VAL : FLOAT) RETURN FLOAT IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN VAL;
                    ELSE
                         RETURN 0.0;
                    END IF;
               END IDENT_FLOAT;

          BEGIN
               IF ONE /= 5 THEN
                    FAILED ("INCORRECT VALUE FOR PARAMETER ONE");
               END IF;
               IF TWO /= 4.5 THEN
                    FAILED ("INCORRECT VALUE FOR PARAMETER TWO");
               END IF;
               IF THREE /= "R1" THEN
                    FAILED ("INCORRECT VALUE FOR PARAMETER THREE");
               END IF;

               IF I /= 7 THEN
                    FAILED ("INCORRECT VALUE FOR OBJECT I");
               END IF;
               D2 := IDENT_FLOAT (3.5);
               IF D2 /= 3.5 THEN
                    FAILED ("INCORRECT VALUE FOR OBJECT D2");
               END IF;
               IF D3 /= "D3" THEN
                    FAILED ("INCORRECT VALUE FOR FUNCTION D3");
               END IF;
          END P;
     BEGIN
          R (D1=>5, D2=>4.5, D3=>"R1");
     END;

     RESULT;
END C83007A;
