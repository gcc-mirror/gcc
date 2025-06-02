-- C74406A.ADA

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
--     CHECK THAT THE FULL DECLARATION OF A LIMITED PRIVATE TYPE CAN
--     DECLARE A TASK TYPE, A TYPE DERIVED FROM A LIMITED PRIVATE TYPE,
--     AND A COMPOSITE TYPE WITH A COMPONENT OF A LIMITED TYPE.

-- HISTORY:
--     BCB 03/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C74406A IS

     PACKAGE TP IS
          TYPE T IS LIMITED PRIVATE;
          PROCEDURE INIT (Z1 : OUT T; Z2 : INTEGER);
          FUNCTION EQUAL_T (ONE, TWO : T) RETURN BOOLEAN;
     PRIVATE
          TYPE T IS RANGE 1 .. 100;
     END TP;

     PACKAGE BODY TP IS
          PROCEDURE INIT (Z1 : OUT T; Z2 : INTEGER) IS
          BEGIN
               Z1 := T (Z2);
          END INIT;

          FUNCTION EQUAL_T (ONE, TWO : T) RETURN BOOLEAN IS
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN ONE = TWO;
               ELSE
                    RETURN ONE /= TWO;
               END IF;
          END EQUAL_T;
     BEGIN
          NULL;
     END TP;

     USE TP;

     PACKAGE P IS
          TYPE T1 IS LIMITED PRIVATE;
          TYPE T2 IS LIMITED PRIVATE;
          TYPE T3 IS LIMITED PRIVATE;
          TYPE T4 IS LIMITED PRIVATE;
     PRIVATE
          TASK TYPE T1 IS
               ENTRY HERE(VAL1 : IN OUT INTEGER);
          END T1;

          TYPE T2 IS NEW T;

          TYPE T3 IS RECORD
               INT : T;
          END RECORD;

          TYPE T4 IS ARRAY(1..5) OF T;
     END P;

     PACKAGE BODY P IS
          X1 : T1;
          X3 : T3;
          X4 : T4;
          VAR : INTEGER := 25;

          TASK BODY T1 IS
          BEGIN
               ACCEPT HERE(VAL1 : IN OUT INTEGER) DO
                    VAL1 := VAL1 * 2;
               END HERE;
          END T1;

     BEGIN
          TEST ("C74406A", "CHECK THAT THE FULL DECLARATION OF A " &
                           "LIMITED PRIVATE TYPE CAN DECLARE A TASK " &
                           "TYPE, A TYPE DERIVED FROM A LIMITED " &
                           "PRIVATE TYPE, AND A COMPOSITE TYPE WITH " &
                           "A COMPONENT OF A LIMITED TYPE");

          X1.HERE(VAR);

          IF NOT EQUAL(VAR,IDENT_INT(50)) THEN
               FAILED ("IMPROPER VALUE FOR VAL");
          END IF;

          INIT (X3.INT, 50);

          IF X3.INT NOT IN T THEN
               FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST");
          END IF;

          INIT (X4(3), 17);

          IF NOT EQUAL_T(T'(X4(3)),T(X4(3))) THEN
               FAILED ("IMPROPER RESULT FROM QUALIFICATION AND " &
                       "EXPLICIT CONVERSION");
          END IF;

          RESULT;
     END P;

     USE P;

BEGIN
     NULL;
END C74406A;
