-- C74203A.ADA

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
--     CHECK THAT MEMBERSHIP TESTS, QUALIFICATION, AND EXPLICIT
--     CONVERSION ARE AVAILABLE FOR LIMITED AND NON-LIMITED PRIVATE
--     TYPES.  INCLUDE TYPES WITH DISCRIMINANTS AND TYPES
--     WITH LIMITED COMPONENTS.

-- HISTORY:
--     BCB 03/10/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C74203A IS

     PACKAGE PP IS
          TYPE LIM IS LIMITED PRIVATE;
          PROCEDURE INIT (Z1 : OUT LIM; Z2 : INTEGER);

          TYPE A IS PRIVATE;
          SUBTYPE SUBA IS A;
          A1 : CONSTANT A;

          TYPE B IS LIMITED PRIVATE;
          B1 : CONSTANT B;

          TYPE C IS PRIVATE;
          C1 : CONSTANT C;

          TYPE D IS LIMITED PRIVATE;
          D1 : CONSTANT D;

          TYPE E (DISC1 : INTEGER := 5) IS PRIVATE;
          SUBTYPE SUBE IS E;
          E1 : CONSTANT E;

          TYPE F (DISC2 : INTEGER := 15) IS LIMITED PRIVATE;
          F1 : CONSTANT F;

          TYPE G (DISC3 : INTEGER) IS PRIVATE;
          G1 : CONSTANT G;

          TYPE H (DISC4 : INTEGER) IS LIMITED PRIVATE;
          H1 : CONSTANT H;

          TYPE I IS RECORD
               COMPI : LIM;
          END RECORD;
          SUBTYPE SUBI IS I;

          TYPE J IS ARRAY(1..5) OF LIM;
          SUBTYPE SUBJ IS J;

          TYPE S1 IS (VINCE, TOM, PHIL, JODIE, ROSA, TERESA);
          TYPE S2 IS (THIS, THAT, THESE, THOSE, THEM);
          TYPE S3 IS RANGE 1 .. 100;
          TYPE S4 IS RANGE 1 .. 100;
     PRIVATE
          TYPE LIM IS RANGE 1 .. 100;

          TYPE A IS (RED, BLUE, GREEN, YELLOW, BLACK, WHITE);
          A1 : CONSTANT A := BLUE;

          TYPE B IS (ONE, TWO, THREE, FOUR, FIVE, SIX);
          B1 : CONSTANT B := THREE;

          TYPE C IS RANGE 1 .. 100;
          C1 : CONSTANT C := 50;

          TYPE D IS RANGE 1 .. 100;
          D1 : CONSTANT D := 50;

          TYPE E (DISC1 : INTEGER := 5) IS RECORD
               COMPE : S1;
          END RECORD;
          E1 : CONSTANT E := (DISC1 => 5, COMPE => TOM);

          TYPE F (DISC2 : INTEGER := 15) IS RECORD
               COMPF : S2;
          END RECORD;
          F1 : CONSTANT F := (DISC2 => 15, COMPF => THAT);

          TYPE G (DISC3 : INTEGER) IS RECORD
               COMPG : S3;
          END RECORD;
          G1 : CONSTANT G := (DISC3 => 25, COMPG => 50);

          TYPE H (DISC4 : INTEGER) IS RECORD
               COMPH : S4;
          END RECORD;
          H1 : CONSTANT H := (DISC4 => 30, COMPH => 50);
     END PP;

     USE PP;

     AVAR : SUBA := A1;
     EVAR : SUBE := E1;

     IVAR : SUBI;
     JVAR : SUBJ;

     PACKAGE BODY PP IS
          PROCEDURE INIT (Z1 : OUT LIM; Z2 : INTEGER) IS
          BEGIN
               Z1 := LIM (Z2);
          END INIT;
     BEGIN
          NULL;
     END PP;

     PROCEDURE QUAL_PRIV (W : A) IS
     BEGIN
          NULL;
     END QUAL_PRIV;

     PROCEDURE QUAL_LIM_PRIV (X : B) IS
     BEGIN
          NULL;
     END QUAL_LIM_PRIV;

     PROCEDURE EXPL_CONV_PRIV_1 (Y : C) IS
     BEGIN
          NULL;
     END EXPL_CONV_PRIV_1;

     PROCEDURE EXPL_CONV_LIM_PRIV_1 (Z : D) IS
     BEGIN
          NULL;
     END EXPL_CONV_LIM_PRIV_1;

     PROCEDURE EXPL_CONV_PRIV_2 (Y2 : G) IS
     BEGIN
          NULL;
     END EXPL_CONV_PRIV_2;

     PROCEDURE EXPL_CONV_LIM_PRIV_2 (Z2 : H) IS
     BEGIN
          NULL;
     END EXPL_CONV_LIM_PRIV_2;

     PROCEDURE EXPL_CONV_PRIV_3 (Y3 : I) IS
     BEGIN
          NULL;
     END EXPL_CONV_PRIV_3;

     PROCEDURE EXPL_CONV_PRIV_4 (Y4 : J) IS
     BEGIN
          NULL;
     END EXPL_CONV_PRIV_4;

BEGIN
     TEST ("C74203A", "CHECK THAT MEMBERSHIP TESTS, QUALIFICATION, " &
                      "AND EXPLICIT CONVERSION ARE AVAILABLE FOR " &
                      "LIMITED AND NON-LIMITED PRIVATE TYPES.  " &
                      "INCLUDE TYPES WITH DISCRIMINANTS AND " &
                      "TYPES WITH LIMITED COMPONENTS");

     INIT (IVAR.COMPI, 50);

     FOR K IN IDENT_INT (1) .. IDENT_INT (5) LOOP
          INIT (JVAR(K), 25);
     END LOOP;

     IF NOT (AVAR IN A) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
                  "PRIVATE TYPE - 1");
     END IF;

     IF (AVAR NOT IN A) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
                  "PRIVATE TYPE - 1");
     END IF;

     IF NOT (B1 IN B) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
                  "LIMITED PRIVATE TYPE - 1");
     END IF;

     IF (B1 NOT IN B) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
                  "LIMITED PRIVATE TYPE - 1");
     END IF;

     QUAL_PRIV (A'(AVAR));

     QUAL_LIM_PRIV (B'(B1));

     EXPL_CONV_PRIV_1 (C(C1));

     EXPL_CONV_LIM_PRIV_1 (D(D1));

     IF NOT (EVAR IN E) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
                  "PRIVATE TYPE - 2");
     END IF;

     IF (EVAR NOT IN E) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
                  "PRIVATE TYPE - 2");
     END IF;

     IF NOT (F1 IN F) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
                  "LIMITED PRIVATE TYPE - 2");
     END IF;

     IF (F1 NOT IN F) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
                  "LIMITED PRIVATE TYPE - 2");
     END IF;

     EXPL_CONV_PRIV_2 (G(G1));

     EXPL_CONV_LIM_PRIV_2 (H(H1));

     IF NOT (IVAR IN I) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
                  "PRIVATE TYPE - 3");
     END IF;

     IF (IVAR NOT IN I) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
                  "PRIVATE TYPE - 3");
     END IF;

     EXPL_CONV_PRIV_3 (I(IVAR));

     IF NOT (JVAR IN J) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'IN' FOR " &
                  "PRIVATE TYPE - 4");
     END IF;

     IF (JVAR NOT IN J) THEN
          FAILED ("IMPROPER RESULT FROM MEMBERSHIP TEST 'NOT IN' FOR " &
                  "PRIVATE TYPE - 4");
     END IF;

     EXPL_CONV_PRIV_4 (J(JVAR));

     RESULT;
END C74203A;
