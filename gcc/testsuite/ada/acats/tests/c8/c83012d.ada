-- C83012D.ADA

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
--     CHECK THAT WITHIN A GENERIC PACKAGE INSTANTIATION, A DECLARATION
--     HAVING THE SAME IDENTIFIER AS THE PACKAGE IS VISIBLE BY
--     SELECTION.

-- HISTORY:
--     JET 08/11/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C83012D IS

     PACKAGE PACK IS
          SUBTYPE PACK1 IS INTEGER;
          PACK2 : INTEGER := 2;
     END PACK;

     TYPE REC IS RECORD
          PACK3 : INTEGER;
          PACK4 : INTEGER;
     END RECORD;

     R : REC := (PACK3 => 3, PACK4 => 1);

     GENERIC
          TYPE T IS RANGE <>;
     PACKAGE GEN1 IS
          J : INTEGER := IDENT_INT(1);
     END GEN1;

     GENERIC
          I : INTEGER;
     PACKAGE GEN2 IS
          J : INTEGER := IDENT_INT(I);
     END GEN2;

     GENERIC
          R : REC;
     PACKAGE GEN3 IS
          J : INTEGER := IDENT_INT(R.PACK4);
     END GEN3;

     GENERIC
          PACK6 : INTEGER;
     PACKAGE GEN4 IS
          J : INTEGER := IDENT_INT(PACK6);
     END GEN4;

     FUNCTION FUNC (PACK5: INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN IDENT_INT(PACK5);
     END FUNC;

     PACKAGE PACK1 IS NEW GEN1(PACK.PACK1);
     PACKAGE PACK2 IS NEW GEN2(PACK.PACK2);
     PACKAGE PACK3 IS NEW GEN2(R.PACK3);
     PACKAGE PACK4 IS NEW GEN3((1, PACK4 => 4));
     PACKAGE PACK5 IS NEW GEN2(FUNC(PACK5 => 5));
     PACKAGE PACK6 IS NEW GEN4(PACK6 => 6);

BEGIN
     TEST ("C83012D", "CHECK THAT WITHIN A GENERIC PACKAGE " &
                      "INSTANTIATION, A DECLARATION HAVING THE SAME " &
                      "IDENTIFIER AS THE PACKAGE IS VISIBLE BY " &
                      "SELECTION");

     IF PACK1.J /= 1 THEN
          FAILED ("INCORRECT VALUE OF PACK1.J");
     END IF;

     IF PACK2.J /= 2 THEN
          FAILED ("INCORRECT VALUE OF PACK2.J");
     END IF;

     IF PACK3.J /= 3 THEN
          FAILED ("INCORRECT VALUE OF PACK3.J");
     END IF;

     IF PACK4.J /= 4 THEN
          FAILED ("INCORRECT VALUE OF PACK4.J");
     END IF;

     IF PACK5.J /= 5 THEN
          FAILED ("INCORRECT VALUE OF PACK5.J");
     END IF;

     IF PACK6.J /= 6 THEN
          FAILED ("INCORRECT VALUE OF PACK6.J");
     END IF;

     RESULT;

END C83012D;
