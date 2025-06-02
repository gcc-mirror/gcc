-- CC1107B.ADA

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
--     CHECK THAT A DEFAULT EXPRESSION MAY REFER TO AN EARLIER FORMAL
--     PARAMETER OF THE SAME GENERIC FORMAL PART.

-- HISTORY:
--     BCB 08/03/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE CC1107B IS

     J, I : INTEGER;

     X : INTEGER := IDENT_INT(0);

     VAL : INTEGER := IDENT_INT(10);

     GENERIC
          X : INTEGER := IDENT_INT(5);
          Y : INTEGER := X;
     FUNCTION F RETURN INTEGER;

     GENERIC
          X : INTEGER;
          Y : INTEGER := X;
     FUNCTION G RETURN INTEGER;

     FUNCTION F RETURN INTEGER IS
     BEGIN
          IF NOT EQUAL(X,Y) THEN
               FAILED ("WRONG VALUE FROM EARLIER FORMAL PARAMETER - 1");
          END IF;

          RETURN 0;
     END F;

     FUNCTION G RETURN INTEGER IS
     BEGIN
          IF NOT EQUAL(X,Y) THEN
               FAILED ("WRONG VALUE FROM EARLIER FORMAL PARAMETER - 2");
          END IF;

          RETURN 0;
     END G;

     FUNCTION NEW_F IS NEW F;

     FUNCTION NEW_G IS NEW G(VAL);

BEGIN
     TEST ("CC1107B", "CHECK THAT A DEFAULT EXPRESSION MAY REFER " &
                      "TO AN EARLIER FORMAL PARAMETER OF THE SAME " &
                      "GENERIC FORMAL PART");

     J := NEW_F;

     I := NEW_G;

     RESULT;
END CC1107B;
