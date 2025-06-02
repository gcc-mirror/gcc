-- C84008A.ADA

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
--     CHECK THAT THE NAMES MADE VISIBLE BY A USE CLAUSE IN THE VISIBLE
--     PART OF A PACKAGE ARE VISIBLE IN THE PRIVATE PART AND BODY OF
--     THE PACKAGE.

-- HISTORY:
--     JET 03/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C84008A IS

     PACKAGE PACK1 IS
          TYPE A IS RANGE 0..100;
          TYPE B IS RANGE -100..0;
     END PACK1;

     PACKAGE PACK2 IS
          USE PACK1;
          TYPE C IS PRIVATE;
          PROCEDURE PROC (X : OUT A; Y : OUT B);
     PRIVATE
          TYPE C IS NEW A RANGE 0..9;
     END PACK2;

     VAR1 : PACK1.A;
     VAR2 : PACK1.B;

     PACKAGE BODY PACK2 IS
          PROCEDURE PROC (X : OUT A; Y : OUT B) IS
               SUBTYPE D IS B RANGE -9..0;
          BEGIN
               IF EQUAL(3,3) THEN
                    X := A'(2);
                    Y := D'(-2);
               ELSE
                    X := A'(0);
                    Y := D'(0);
               END IF;
          END PROC;
     END PACK2;

BEGIN
     TEST ("C84008A", "CHECK THAT THE NAMES MADE VISIBLE BY A USE " &
                      "CLAUSE IN THE VISIBLE PART OF A PACKAGE ARE " &
                      "VISIBLE IN THE PRIVATE PART AND BODY OF " &
                      "THE PACKAGE");

     PACK2.PROC (VAR1,VAR2);

     IF PACK1."/=" (VAR1, 2) THEN
          FAILED("INCORRECT RETURN VALUE FOR VAR1");
     END IF;

     IF PACK1."/=" (VAR2, PACK1."-"(2)) THEN
          FAILED("INCORRECT RETURN VALUE FOR VAR2");
     END IF;

     RESULT;
END C84008A;
