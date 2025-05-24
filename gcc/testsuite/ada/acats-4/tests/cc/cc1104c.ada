-- CC1104C.ADA

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
-- OBJECTIVE;
--     CHECK THAT A GENERIC FORMAL IN OUT PARAMETER CAN HAVE A
--     LIMITED TYPE.

-- HISTORY:
--     BCB 08/03/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE CC1104C IS

     TASK TYPE TSK IS
          ENTRY E;
     END TSK;

     VAR : INTEGER := IDENT_INT(0);
     NEW_VAL : INTEGER := IDENT_INT(5);

     TSK_VAR : TSK;

     PACKAGE PP IS
          TYPE LP IS LIMITED PRIVATE;
          PROCEDURE INIT (ONE : OUT LP; TWO : INTEGER);
          FUNCTION EQUAL (ONE : LP; TWO : INTEGER) RETURN BOOLEAN;
     PRIVATE
          TYPE LP IS RANGE 1 .. 100;
     END PP;

     USE PP;

     TYPE REC IS RECORD
          COMP : LP;
     END RECORD;

     C : LP;

     REC_VAR : REC;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
          IN_OUT_VAR : IN OUT T;
          IN_OUT_TSK : IN OUT TSK;
          VAL : IN OUT T;
          WITH PROCEDURE INIT (L : IN OUT T; R : T);
     PROCEDURE P;

     GENERIC
          VAL : IN OUT LP;
     PROCEDURE Q;

     GENERIC
          VAL : IN OUT REC;
     PROCEDURE R;

     PACKAGE BODY PP IS
          PROCEDURE INIT(ONE : OUT LP; TWO : INTEGER) IS
          BEGIN
               ONE := LP(TWO);
          END INIT;

          FUNCTION EQUAL(ONE : LP; TWO : INTEGER) RETURN BOOLEAN IS
          BEGIN
               RETURN ONE = LP(TWO);
          END EQUAL;
     END PP;

     TASK BODY TSK IS
     BEGIN
          ACCEPT E;
     END TSK;

     PROCEDURE P IS
     BEGIN
          INIT(IN_OUT_VAR,VAL);
          IN_OUT_TSK.E;
          INIT(C,50);
     END P;

     PROCEDURE Q IS
     BEGIN
          INIT(VAL,75);
          INIT(REC_VAR.COMP,50);
     END Q;

     PROCEDURE R IS
     BEGIN
          INIT(VAL.COMP,75);
     END R;

     PROCEDURE I (ONE : IN OUT INTEGER; TWO : INTEGER) IS
     BEGIN
          ONE := TWO;
     END I;

     PROCEDURE NEW_P IS NEW P(INTEGER,VAR,TSK_VAR,NEW_VAL,I);

     PROCEDURE NEW_Q IS NEW Q(C);

     PROCEDURE NEW_R IS NEW R(REC_VAR);

BEGIN
     TEST ("CC1104C", "CHECK THAT A GENERIC FORMAL IN OUT PARAMETER " &
                      "CAN HAVE A LIMITED TYPE");

     NEW_P;

     IF NOT EQUAL(VAR,5) THEN
          FAILED ("WRONG VALUE ASSIGNED TO IN OUT PARAMETER IN " &
                  "GENERIC PACKAGE - 1");
     END IF;

     NEW_Q;

     IF NOT EQUAL(C,75) THEN
          FAILED ("WRONG VALUE ASSIGNED TO IN OUT PARAMETER IN " &
                  "GENERIC PACKAGE - 2");
     END IF;

     NEW_R;

     IF NOT EQUAL(REC_VAR.COMP,75) THEN
          FAILED ("WRONG VALUE ASSIGNED TO IN OUT PARAMETER IN " &
                  "GENERIC PACKAGE - 3");
     END IF;

     RESULT;
END CC1104C;
