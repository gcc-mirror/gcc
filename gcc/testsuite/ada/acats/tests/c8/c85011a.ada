-- C85011A.ADA

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
--     CHECK THAT A PACKAGE CAN BE RENAMED AND THE NEW NAME CAN APPEAR
--     IN A RENAMING DECLARATION, AND THAT A 'USE' CLAUSE CAN REFER TO
--     THE PACKAGE BY EITHER NAME, INCLUDING RENAMINGS OF GENERIC AND
--     NONGENERIC PACKAGES INSIDE THEMSELVES.

-- HISTORY:
--     JET 04/28/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C85011A IS

     PACKAGE PACK1 IS
          I : NATURAL := 0;
          PACKAGE PACKA RENAMES PACK1;
     END PACK1;

     GENERIC
          TYPE T IS RANGE <>;
     PACKAGE GPACK IS
          J : T := T'FIRST;
          PACKAGE PACKB RENAMES GPACK;
     END GPACK;

     PACKAGE PACK2 IS NEW GPACK(NATURAL);

     PACKAGE PACK3 RENAMES PACK1;
     PACKAGE PACK4 RENAMES PACK2;
     PACKAGE PACK5 RENAMES PACK3;
     PACKAGE PACK6 RENAMES PACK4;

BEGIN
     TEST ("C85011A", "CHECK THAT A PACKAGE CAN BE RENAMED AND THE " &
                      "NEW NAME CAN APPEAR IN A RENAMING " &
                      "DECLARATION, AND THAT A 'USE' CLAUSE CAN " &
                      "REFER TO THE PACKAGE BY EITHER NAME, " &
                      "INCLUDING RENAMINGS OF GENERIC AND NONGENERIC " &
                      "PACKAGES INSIDE THEMSELVES");

     IF PACK1.I /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK1.I");
     END IF;

     IF PACK2.J /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK2.J");
     END IF;

     IF PACK3.I /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK3.I");
     END IF;

     IF PACK4.J /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK4.J");
     END IF;

     IF PACK5.I /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK5.I");
     END IF;

     IF PACK6.J /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK6.J");
     END IF;

     IF PACK1.PACKA.I /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK1.PACKA.I");
     END IF;

     IF PACK2.PACKB.J /= IDENT_INT(0) THEN
          FAILED ("INCORRECT VALUE OF PACK2.PACKB.J");
     END IF;

     DECLARE
          USE PACK1, PACK2;
     BEGIN
          IF I /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF I (1)");
          END IF;

          IF J /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF J (1)");
          END IF;
     END;

     DECLARE
          USE PACK3, PACK4;
     BEGIN
          IF I /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF I (2)");
          END IF;

          IF J /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF J (2)");
          END IF;
     END;

     DECLARE
          USE PACK5, PACK6;
     BEGIN
          IF I /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF I (3)");
          END IF;

          IF J /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF J (3)");
          END IF;
     END;

     DECLARE
          USE PACK1.PACKA, PACK2.PACKB;
     BEGIN
          IF I /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF I (4)");
          END IF;

          IF J /= IDENT_INT(0) THEN
               FAILED ("INCORRECT VALUE OF J (4)");
          END IF;
     END;

     RESULT;
END C85011A;
