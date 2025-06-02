-- CC1307B.ADA

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
--     CHECK THAT AN ENUMERATION LITERAL (BOTH AN IDENTIFIER AND A
--     CHARACTER LITERAL) MAY BE USED AS A DEFAULT SUBPROGRAM NAME
--     AND AS A DEFAULT INITIAL VALUE FOR AN OBJECT PARAMETER.

-- HISTORY:
--     BCB 08/09/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE CC1307B IS

     TYPE ENUM IS (R, 'S', R1);

BEGIN
     TEST ("CC1307B", "CHECK THAT AN ENUMERATION LITERAL (BOTH AN " &
                      "IDENTIFIER AND A CHARACTER LITERAL) MAY BE " &
                      "USED AS A DEFAULT SUBPROGRAM NAME AND AS A " &
                      "DEFAULT INITIAL VALUE FOR AN OBJECT PARAMETER");

     DECLARE
          GENERIC
               WITH FUNCTION J RETURN ENUM IS R;
               WITH FUNCTION K RETURN ENUM IS 'S';
               OBJ1 : ENUM := R;
               OBJ2 : ENUM := 'S';
          PACKAGE P IS
          END P;

          PACKAGE BODY P IS
               VAR1, VAR2 : ENUM := R1;
          BEGIN
               VAR1 := J;

               IF VAR1 /= R THEN
                    FAILED ("WRONG VALUE FOR DEFAULT SUBPROGRAM " &
                            "NAME - IDENTIFIER");
               END IF;

               VAR2 := K;

               IF VAR2 /= 'S' THEN
                    FAILED ("WRONG VALUE FOR DEFAULT SUBPROGRAM " &
                            "NAME - CHARACTER LITERAL");
               END IF;

               IF OBJ1 /= R THEN
                    FAILED ("WRONG VALUE FOR OBJECT PARAMETER - " &
                            "IDENTIFIER");
               END IF;

               IF OBJ2 /= 'S' THEN
                    FAILED ("WRONG VALUE FOR OBJECT PARAMETER - " &
                            "CHARACTER LITERAL");
               END IF;
          END P;

          PACKAGE NEW_P IS NEW P;
     BEGIN
          NULL;
     END;

     RESULT;
END CC1307B;
