-- C43004C.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE VALUE OF A
--     DISCRIMINANT OF A CONSTRAINED COMPONENT OF AN AGGREGATE DOES
--     NOT EQUAL THE CORRESPONDING DISCRIMINANT VALUE FOR THE
--     COMPONENT'S SUBTYPE.

-- HISTORY:
--     BCB 07/19/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C43004C IS

     ZERO : INTEGER := 0;

     TYPE REC (D : INTEGER := 0) IS RECORD
          COMP1 : INTEGER;
     END RECORD;

     TYPE DREC (DD : INTEGER := ZERO) IS RECORD
          DCOMP1 : INTEGER;
     END RECORD;

     TYPE REC1 IS RECORD
          A : REC(0);
     END RECORD;

     TYPE REC2 IS RECORD
          B : DREC(ZERO);
     END RECORD;

     TYPE REC3 (D3 : INTEGER := 0) IS RECORD
          C : REC(D3);
     END RECORD;

     V : REC1;
     W : REC2;
     X : REC3;

     PACKAGE P IS
          TYPE PRIV1 (D : INTEGER := 0) IS PRIVATE;
          TYPE PRIV2 (DD : INTEGER := ZERO) IS PRIVATE;
          FUNCTION INIT (I : INTEGER) RETURN PRIV1;
     PRIVATE
          TYPE PRIV1 (D : INTEGER := 0) IS RECORD
               NULL;
          END RECORD;

          TYPE PRIV2 (DD : INTEGER := ZERO) IS RECORD
               NULL;
          END RECORD;
     END P;

     TYPE REC7 IS RECORD
          H : P.PRIV1 (0);
     END RECORD;

     Y : REC7;

     GENERIC
          TYPE GP IS PRIVATE;
     FUNCTION GEN_EQUAL (X, Y : GP) RETURN BOOLEAN;

     FUNCTION GEN_EQUAL (X, Y : GP) RETURN BOOLEAN IS
     BEGIN
          RETURN X = Y;
     END GEN_EQUAL;

     PACKAGE BODY P IS
          TYPE REC4 IS RECORD
               E : PRIV1(0);
          END RECORD;

          TYPE REC5 IS RECORD
               F : PRIV2(ZERO);
          END RECORD;

          TYPE REC6 (D6 : INTEGER := 0) IS RECORD
               G : PRIV1(D6);
          END RECORD;

          VV : REC4;
          WW : REC5;
          XX : REC6;

          FUNCTION REC4_EQUAL IS NEW GEN_EQUAL (REC4);
          FUNCTION REC5_EQUAL IS NEW GEN_EQUAL (REC5);
          FUNCTION REC6_EQUAL IS NEW GEN_EQUAL (REC6);

          FUNCTION INIT (I : INTEGER) RETURN PRIV1 IS
               VAR : PRIV1;
          BEGIN
               VAR := (D => I);
               RETURN VAR;
          END INIT;
     BEGIN
          TEST ("C43004C", "CHECK THAT CONSTRAINT_ERROR IS RAISED " &
                           "IF THE VALUE OF A DISCRIMINANT OF A " &
                           "CONSTRAINED COMPONENT OF AN AGGREGATE " &
                           "DOES NOT EQUAL THE CORRESPONDING " &
                            "DISCRIMINANT VALUE FOR THECOMPONENT'S " &
                           "SUBTYPE");

          BEGIN
               VV := (E => (D => 1));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 1");
               IF REC4_EQUAL (VV,VV) THEN
                    COMMENT ("DON'T OPTIMIZE VV");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 1");
          END;

          BEGIN
               WW := (F => (DD => 1));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 2");
               IF REC5_EQUAL (WW,WW) THEN
                    COMMENT ("DON'T OPTIMIZE WW");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 2");
          END;

          BEGIN
               XX := (D6 => 1, G => (D => 5));
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 3");
               IF REC6_EQUAL (XX,XX) THEN
                    COMMENT ("DON'T OPTIMIZE XX");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("OTHER EXCEPTION RAISED - 3");
          END;
     END P;

     USE P;

     FUNCTION REC1_EQUAL IS NEW GEN_EQUAL (REC1);
     FUNCTION REC2_EQUAL IS NEW GEN_EQUAL (REC2);
     FUNCTION REC3_EQUAL IS NEW GEN_EQUAL (REC3);
     FUNCTION REC7_EQUAL IS NEW GEN_EQUAL (REC7);

BEGIN

     BEGIN
          V := (A => (D => 1, COMP1 => 2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 4");
          IF REC1_EQUAL (V,V) THEN
               COMMENT ("DON'T OPTIMIZE V");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 4");
     END;

     BEGIN
          W := (B => (DD => 1, DCOMP1 => 2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 5");
          IF REC2_EQUAL (W,W) THEN
               COMMENT ("DON'T OPTIMIZE W");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 5");
     END;

     BEGIN
          X := (D3 => 1, C => (D => 5, COMP1 => 2));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 6");
          IF REC3_EQUAL (X,X) THEN
               COMMENT ("DON'T OPTIMIZE X");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 6");
     END;

     BEGIN
          Y := (H => INIT (1));
          FAILED ("CONSTRAINT_ERROR NOT RAISED - 7");
          IF REC7_EQUAL (Y,Y) THEN
               COMMENT ("DON'T OPTIMIZE Y");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION RAISED - 7");
     END;

     RESULT;
END C43004C;
