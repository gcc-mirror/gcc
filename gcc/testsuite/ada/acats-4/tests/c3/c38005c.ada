-- C38005C.ADA

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
--     CHECK THAT ALL OBJECTS OF FORMAL ACCESS TYPE, INCLUDING ARRAY AND
--     RECORD COMPONENTS, ARE INITIALIZED BY DEFAULT WITH THE VALUE
--     NULL.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C38005C IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 10;

     TYPE ACC_I IS ACCESS INT;

     SUBTYPE NEW_NODE IS CHARACTER;

     TYPE ACC_CHAR IS ACCESS NEW_NODE;

     X : ACC_I := NEW INT'(IDENT_INT(5));
     Y : NEW_NODE := 'A';
     Z : ACC_CHAR := NEW NEW_NODE'(Y);

     GENERIC
          TYPE ACC_INT IS ACCESS INT;
          TYPE NODE IS PRIVATE;
          TYPE LINK IS ACCESS NODE;
     PROCEDURE P(U : ACC_INT; V : NODE; W : LINK);

     GENERIC
          TYPE ACC_INT IS ACCESS INT;
          TYPE NODE IS PRIVATE;
          TYPE LINK IS ACCESS NODE;
     PACKAGE PACK IS

          SUBTYPE NEW_ACC IS ACC_INT;

          SUBTYPE NEW_L IS LINK;

          TYPE ARR IS ARRAY(1 .. 4) OF ACC_INT;

          TYPE REC IS
               RECORD
                    I : ACC_INT;
                    L : LINK;
               END RECORD;

     END PACK;

     PACKAGE NEW_PACK IS NEW PACK(ACC_I, NEW_NODE, ACC_CHAR);
     USE NEW_PACK;

     A : NEW_PACK.NEW_ACC;
     B : NEW_PACK.NEW_L;
     C : NEW_PACK.ARR;
     D : NEW_PACK.REC;

     PROCEDURE P(U : ACC_INT; V : NODE; W : LINK) IS

          TYPE ARR IS ARRAY(1 .. 4) OF ACC_INT;

          TYPE REC IS
               RECORD
                    I : ACC_INT;
                    L : LINK;
               END RECORD;

          A : ACC_INT;
          B : LINK;
          C : ARR;
          D : REC;

     BEGIN
          IF A /= NULL THEN
               FAILED("OBJECT A NOT INITIALIZED - PROC");
          END IF;

          IF B /= NULL THEN
               FAILED("OBJECT B NOT INITIALIZED - PROC");
          END IF;

          FOR I IN 1 .. 4 LOOP
               IF C(I) /= NULL THEN
                    FAILED("ARRAY " & INTEGER'IMAGE(I) &
                           "NOT INITIALIZED - PROC");
               END IF;
          END LOOP;

          IF D.I /= NULL THEN
               FAILED("RECORD.I NOT INITIALIZED - PROC");
          END IF;

          IF D.L /= NULL THEN
               FAILED("RECORD.L NOT INITIALIZED - PROC");
          END IF;

     END P;

     PROCEDURE PROC IS NEW P(ACC_I, NEW_NODE, ACC_CHAR);

BEGIN
     TEST("C38005C", "CHECK THAT ALL OBJECTS OF FORMAL ACCESS TYPE, " &
                     "INCLUDING ARRAY AND RECORD COMPONENTS, ARE " &
                     "INITIALIZED BY DEFAULT WITH THE VALUE NULL");

     PROC(X, Y, Z);

     IF A /= NULL THEN
          FAILED("OBJECT A NOT INITIALIZED - PACK");
     END IF;

     IF B /= NULL THEN
          FAILED("OBJECT B NOT INITIALIZED - PACK");
     END IF;

     FOR I IN 1 .. 4 LOOP
          IF C(I) /= NULL THEN
               FAILED("ARRAY " & INTEGER'IMAGE(I) &
                      "NOT INITIALIZED - PACK");
          END IF;
     END LOOP;

     IF D.I /= NULL THEN
          FAILED("RECORD.I NOT INITIALIZED - PACK");
     END IF;

     IF D.L /= NULL THEN
          FAILED("RECORD.L NOT INITIALIZED - PACK");
     END IF;

     RESULT;
END C38005C;
