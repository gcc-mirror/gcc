-- C46021A.ADA

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
--     CHECK THAT FLOATING POINT CONVERSIONS ARE PERFORMED CORRECTLY
--     WHEN THE OPERAND TYPE IS AN INTEGER TYPE, FOR 5-DIGIT PRECISION.

-- HISTORY:
--     JET 02/12/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C46021A IS

     TYPE FLOAT5 IS DIGITS 5;
     TYPE INT IS RANGE -32768..32767;

     TYPE NFLOAT5 IS NEW FLOAT5;

     FUNCTION IDENT (A : FLOAT5) RETURN FLOAT5 IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN A;
          ELSE
               RETURN 0.0;
          END IF;
     END IDENT;

     FUNCTION IDENT (A : NFLOAT5) RETURN NFLOAT5 IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN A;
          ELSE
               RETURN 0.0;
          END IF;
     END IDENT;

BEGIN
     TEST ("C46021A", "CHECK THAT FLOATING POINT CONVERSIONS ARE " &
                      "PERFORMED CORRECTLY WHEN THE OPERAND TYPE " &
                      "IS AN INTEGER TYPE, FOR 5-DIGIT PRECISION");

     IF FLOAT5(IDENT_INT(-7)) /= -7.0 THEN
          FAILED ("INCORRECT VALUE (1)");
     END IF;

     IF FLOAT5(IDENT_INT(3)) /= 3.0 THEN
          FAILED ("INCORRECT VALUE (2)");
     END IF;

     IF FLOAT5(IDENT_INT(-999)) /= -999.0 THEN
          FAILED ("INCORRECT VALUE (3)");
     END IF;

     IF FLOAT5(IDENT_INT(101)) /= 101.0 THEN
          FAILED ("INCORRECT VALUE (4)");
     END IF;

     IF FLOAT5(IDENT_INT(-32767)) /= -32767.0 THEN
          FAILED ("INCORRECT VALUE (5)");
     END IF;

     IF FLOAT5(IDENT_INT(32767)) /= 32767.0 THEN
          FAILED ("INCORRECT VALUE (6)");
     END IF;

     IF FLOAT5(-7) /= IDENT(-7.0) THEN
          FAILED ("INCORRECT VALUE (7)");
     END IF;

     IF FLOAT5(3) /= IDENT(3.0) THEN
          FAILED ("INCORRECT VALUE (8)");
     END IF;

     IF FLOAT5(-999) /= IDENT(-999.0) THEN
          FAILED ("INCORRECT VALUE (9)");
     END IF;

     IF FLOAT5(101) /= IDENT(101.0) THEN
          FAILED ("INCORRECT VALUE (10)");
     END IF;

     IF FLOAT5(-32767) /= IDENT(-32767.0) THEN
          FAILED ("INCORRECT VALUE (11)");
     END IF;

     IF FLOAT5(32767) /= IDENT(32767.0) THEN
          FAILED ("INCORRECT VALUE (12)");
     END IF;

     IF FLOAT5(INT'(-7)) /= IDENT(-7.0) THEN
          FAILED ("INCORRECT VALUE (13)");
     END IF;

     IF FLOAT5(INT'(3)) /= IDENT(3.0) THEN
          FAILED ("INCORRECT VALUE (14)");
     END IF;

     IF FLOAT5(INT'(-999)) /= IDENT(-999.0) THEN
          FAILED ("INCORRECT VALUE (15)");
     END IF;

     IF FLOAT5(INT'(101)) /= IDENT(101.0) THEN
          FAILED ("INCORRECT VALUE (16)");
     END IF;

     IF FLOAT5(INT'(-32767)) /= IDENT(-32767.0) THEN
          FAILED ("INCORRECT VALUE (17)");
     END IF;

     IF FLOAT5(INT'(32767)) /= IDENT(32767.0) THEN
          FAILED ("INCORRECT VALUE (18)");
     END IF;

     IF NFLOAT5(IDENT_INT(-7)) /= -7.0 THEN
          FAILED ("INCORRECT VALUE (19)");
     END IF;

     IF NFLOAT5(IDENT_INT(3)) /= 3.0 THEN
          FAILED ("INCORRECT VALUE (20)");
     END IF;

     IF NFLOAT5(IDENT_INT(-999)) /= -999.0 THEN
          FAILED ("INCORRECT VALUE (21)");
     END IF;

     IF NFLOAT5(IDENT_INT(101)) /= 101.0 THEN
          FAILED ("INCORRECT VALUE (22)");
     END IF;

     IF NFLOAT5(IDENT_INT(-32767)) /= -32767.0 THEN
          FAILED ("INCORRECT VALUE (23)");
     END IF;

     IF NFLOAT5(IDENT_INT(32767)) /= 32767.0 THEN
          FAILED ("INCORRECT VALUE (24)");
     END IF;

     IF NFLOAT5(-7) /= IDENT(-7.0) THEN
          FAILED ("INCORRECT VALUE (25)");
     END IF;

     IF NFLOAT5(3) /= IDENT(3.0) THEN
          FAILED ("INCORRECT VALUE (26)");
     END IF;

     IF NFLOAT5(-999) /= IDENT(-999.0) THEN
          FAILED ("INCORRECT VALUE (27)");
     END IF;

     IF NFLOAT5(101) /= IDENT(101.0) THEN
          FAILED ("INCORRECT VALUE (28)");
     END IF;

     IF NFLOAT5(-32767) /= IDENT(-32767.0) THEN
          FAILED ("INCORRECT VALUE (29)");
     END IF;

     IF NFLOAT5(32767) /= IDENT(32767.0) THEN
          FAILED ("INCORRECT VALUE (30)");
     END IF;

     IF NFLOAT5(INT'(-7)) /= IDENT(-7.0) THEN
          FAILED ("INCORRECT VALUE (31)");
     END IF;

     IF NFLOAT5(INT'(3)) /= IDENT(3.0) THEN
          FAILED ("INCORRECT VALUE (32)");
     END IF;

     IF NFLOAT5(INT'(-999)) /= IDENT(-999.0) THEN
          FAILED ("INCORRECT VALUE (33)");
     END IF;

     IF NFLOAT5(INT'(101)) /= IDENT(101.0) THEN
          FAILED ("INCORRECT VALUE (34)");
     END IF;

     IF NFLOAT5(INT'(-32767)) /= IDENT(-32767.0) THEN
          FAILED ("INCORRECT VALUE (35)");
     END IF;

     IF NFLOAT5(INT'(32767)) /= IDENT(32767.0) THEN
          FAILED ("INCORRECT VALUE (36)");
     END IF;

     RESULT;

END C46021A;
