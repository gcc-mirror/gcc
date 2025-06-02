-- CC3120A.ADA

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
-- CHECK THAT GENERIC IN PARAMETERS ARE ALWAYS COPIED, AND THAT
-- GENERIC IN OUT PARAMETERS ARE ALWAYS RENAMED.

-- DAT 8/10/81
-- SPS 10/21/82

WITH REPORT; USE REPORT;

PROCEDURE CC3120A IS
BEGIN
     TEST ("CC3120A", "GENERIC IN PARMS ARE COPIED, GENERIC IN OUT"
          & " PARMS ARE RENAMED");

     DECLARE
          S1, S2 : INTEGER;
          A1, A2, A3 : STRING (1 .. IDENT_INT (3));

          TYPE REC IS RECORD
               C1, C2 : INTEGER := 1;
          END RECORD;

          R1, R2 : REC;

          PACKAGE P IS
               TYPE PRIV IS PRIVATE;
               PROCEDURE SET_PRIV (P : IN OUT PRIV);
          PRIVATE
               TYPE PRIV IS NEW REC;
          END P;
          USE P;

          P1, P2 : PRIV;
          EX : EXCEPTION;

          GENERIC
               TYPE T IS PRIVATE;
               P1 : IN OUT T;
               P2 : IN T;
          PROCEDURE GP;

          B_ARR : ARRAY (1..10) OF BOOLEAN;

          PACKAGE BODY P IS
               PROCEDURE SET_PRIV (P : IN OUT PRIV) IS
               BEGIN
                    P.C1 := 3;
               END SET_PRIV;
          END P;

          PROCEDURE GP IS
          BEGIN
               IF P1 = P2 THEN
                    FAILED ("PARAMETER SCREW_UP SOMEWHERE");
               END IF;
               P1 := P2;
               IF P1 /= P2 THEN
                    FAILED ("ASSIGNMENT SCREW_UP SOMEWHERE");
               END IF;
               RAISE EX;
               FAILED ("RAISE STATEMENT DOESN'T WORK");
          END GP;
     BEGIN
          S1 := 4;
          S2 := 5;
          A1 := "XYZ";
          A2 := "ABC";
          A3 := "DEF";
          R1.C1 := 4;
          R2.C1 := 5;
          B_ARR := (1|3|5|7|9 => TRUE, 2|4|6|8|10 => FALSE);
          SET_PRIV (P2);

          IF S1 = S2
          OR A1 = A3
          OR R1 = R2
          OR P1 = P2 THEN
               FAILED ("WRONG ASSIGNMENT");
          END IF;
          BEGIN
               DECLARE
                    PROCEDURE PR IS NEW GP (INTEGER, S1, S2);
               BEGIN
                    S2 := S1;
                    PR;       -- OLD S2 ASSIGNED TO S1, SO S1 /= S2 NOW
                    FAILED ("EX NOT RAISED 1");
               EXCEPTION
                    WHEN EX => NULL;
               END;

               DECLARE
                    SUBTYPE STR_1_3 IS STRING (IDENT_INT (1)..3);
                    PROCEDURE PR IS NEW GP (STR_1_3, A1, A3);
               BEGIN
                    A3 := A1;
                    PR;
                    FAILED ("EX NOT RAISED 2");
               EXCEPTION
                    WHEN EX => NULL;
               END;

               DECLARE
                    PROCEDURE PR IS NEW GP (REC, R1, R2);
               BEGIN
                    R2 := R1;
                    PR;
                    FAILED ("EX NOT RAISED 3");
               EXCEPTION
                    WHEN EX => NULL;
               END;

               DECLARE
                    PROCEDURE PR IS NEW GP (PRIV, P1, P2);
               BEGIN
                    P2 := P1;
                    PR;
                    FAILED ("EX NOT RAISED 4");
               EXCEPTION
                    WHEN EX => NULL;
               END;
               DECLARE
                    PROCEDURE PR IS NEW GP (CHARACTER,
                                            A3(IDENT_INT(2)),
                                            A3(IDENT_INT(3)));
               BEGIN
                    A3(3) := A3(2);
                    PR;
                    FAILED ("EX NOT RAISED 5");
               EXCEPTION
                    WHEN EX => NULL;
               END;

               DECLARE
                    PROCEDURE PR IS NEW GP (BOOLEAN,
                                            B_ARR(IDENT_INT(2)),
                                            B_ARR(IDENT_INT(3)));
               BEGIN
                    B_ARR(3) := B_ARR(2);
                    PR;
                    FAILED ("EX NOT RAISED 6");
               EXCEPTION
                    WHEN EX => NULL;
               END;
          END;

          IF S1 = S2
          OR A1 = A2
          OR R1 = R2
          OR P1 = P2
          OR A3(2) = A3(3) 
          OR B_ARR(2) = B_ARR(3) THEN
               FAILED ("ASSIGNMENT FAILED 2");
          END IF;
     END;

     RESULT;
END CC3120A;
