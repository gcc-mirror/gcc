-- C55B11A.ADA

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
--     CHECK THAT, IN 'FOR IN ST RANGE L .. R LOOP', THE PARAMETER IS OF
--     THE TYPE ST'BASE; THAT IS THAT IT CAN BE ASSIGNED TO OTHER
--     VARIABLES DECLARED WITH SOME OTHER SUBTYPES OF ST.

-- HISTORY:
--     DHH 08/15/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C55B11A IS

     TYPE ENUM IS (A, B, C, D, E, F, G, H);

     SUBTYPE ONE IS ENUM RANGE A .. H;
     SUBTYPE TWO IS ENUM RANGE B .. H;
     SUBTYPE THREE IS ENUM RANGE C .. H;
     SUBTYPE FOUR IS ENUM RANGE D .. H;

     GLOBAL : INTEGER := 0;

     VAR_1 : ONE;
     VAR_2 : TWO;
     VAR_3 : THREE;
     VAR_4 : FOUR;

     PROCEDURE CHECK_VAR(T : ENUM) IS
     BEGIN
          GLOBAL := GLOBAL + 1;
          CASE T IS
               WHEN D =>
                    IF GLOBAL /= IDENT_INT(1) THEN
                         FAILED("VAR_1 WRONG VALUE");
                    END IF;

               WHEN E =>
                    IF GLOBAL /= IDENT_INT(2) THEN
                         FAILED("VAR_2 WRONG VALUE");
                    END IF;

               WHEN F =>
                    IF GLOBAL /= IDENT_INT(3) THEN
                         FAILED("VAR_3 WRONG VALUE");
                    END IF;

               WHEN G =>
                    IF GLOBAL /= IDENT_INT(4) THEN
                         FAILED("VAR_4 WRONG VALUE");
                    END IF;

               WHEN OTHERS =>

                   FAILED("WRONG VALUE TO PROCEDURE");
          END CASE;
     END CHECK_VAR;

BEGIN
     TEST("C55B11A", "CHECK THAT, IN 'FOR IN ST RANGE L .. R LOOP', " &
                     "THE PARAMETER IS OF THE TYPE ST'BASE; THAT IS " &
                     "THAT IT CAN BE ASSIGNED TO OTHER VARIABLES " &
                     "DECLARED WITH SOME OTHER SUBTYPES OF ST");

     FOR I IN ONE RANGE D .. G LOOP
          CASE I IS
               WHEN D =>
                    VAR_1 := I;
                    CHECK_VAR(VAR_1);
               WHEN E =>
                    VAR_2 := I;
                    CHECK_VAR(VAR_2);
               WHEN F =>
                    VAR_3 := I;
                    CHECK_VAR(VAR_3);
               WHEN G =>
                    VAR_4 := I;
                    CHECK_VAR(VAR_4);
          END CASE;
     END LOOP;

     RESULT;
END C55B11A;
