-- C74401E.ADA

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
-- CHECK THAT OUT PARAMETERS HAVING A LIMITED PRIVATE TYPE CAN BE
-- DECLARED IN A PACKAGE SPECIFICATION, INCLUDING WITHIN PACKAGES
-- NESTED IN A VISIBLE PART.

-- CHECK THAT A RENAMING DECLARATION CAN RENAME A PROCEDURE DECLARED
-- WITH AN OUT PARAMETER.

-- JBG 5/1/85

WITH REPORT; USE REPORT;
PROCEDURE C74401E IS

     PACKAGE PKG IS
          TYPE LP IS LIMITED PRIVATE;
          PROCEDURE P20 (X : OUT LP);        -- OK.
          PROCEDURE RESET (X : OUT LP);
          FUNCTION EQ (L, R : LP) RETURN BOOLEAN;
          VAL1 : CONSTANT LP;

          PACKAGE NESTED IS
               PROCEDURE NEST1 (X : OUT LP);
          PRIVATE
               PROCEDURE NEST2 (X : OUT LP);
          END NESTED;
     PRIVATE
          TYPE LP IS NEW INTEGER;
          VAL1 : CONSTANT LP := LP(IDENT_INT(3));
     END PKG;

     VAR : PKG.LP;

     PACKAGE BODY PKG IS
          PROCEDURE P20 (X : OUT LP) IS
          BEGIN
               X := 3;
          END P20;

          PROCEDURE RESET (X : OUT LP) IS
          BEGIN
               X := LP(IDENT_INT(0));
          END RESET;

          FUNCTION EQ (L, R : LP) RETURN BOOLEAN IS
          BEGIN
               RETURN L = R;
          END EQ;

          PACKAGE BODY NESTED IS
               PROCEDURE NEST1 (X : OUT LP) IS
               BEGIN
                    X := 3;
               END NEST1;

               PROCEDURE NEST2 (X : OUT LP) IS
               BEGIN
                    X := LP(IDENT_INT(3));
               END NEST2;
          END NESTED;
     BEGIN
          VAR := LP(IDENT_INT(0));
     END PKG;

     PACKAGE PKG1 IS
          PROCEDURE P21 (X : OUT PKG.LP) RENAMES PKG.P20;   -- OK:
                                                            -- RENAMING.
     END PKG1;

BEGIN

     TEST ("C74401E", "CHECK THAT A PROCEDURE CAN HAVE AN OUT " &
                      "PARAMETER WITH A LIMITED PRIVATE TYPE");

     PKG.RESET (VAR);
     PKG.P20 (VAR);

     IF NOT PKG.EQ (VAR, PKG.VAL1) THEN
          FAILED ("DIRECT CALL NOT CORRECT");
     END IF;

     PKG.RESET (VAR);
     PKG1.P21 (VAR);

     IF NOT PKG.EQ (VAR, PKG.VAL1) THEN
          FAILED ("RENAMED CALL NOT CORRECT");
     END IF;

     PKG.RESET (VAR);
     PKG.NESTED.NEST1 (VAR);

     IF NOT PKG.EQ (VAR, PKG.VAL1) THEN
          FAILED ("NESTED CALL NOT CORRECT");
     END IF;

     RESULT;

END C74401E;
