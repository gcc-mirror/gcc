-- C48009G.ADA

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
--     FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT
--     CONSTRAINT_ERROR IS RAISED IF T IS A CONSTRAINED ACCESS
--     TYPE AND THE OBJECT DESIGNATED BY X DOES NOT HAVE DISCRIMINANTS
--     OR INDEX BOUNDS THAT EQUAL THE CORRESPONDING VALUES FOR T.

-- HISTORY:
--     EG  08/30/84  CREATED ORIGINAL TEST.
--     JET 01/05/87  UPDATED HEADER FORMAT AND ADDED CODE TO PREVENT
--                   OPTIMIZATION.

WITH REPORT;

PROCEDURE C48009G IS

     USE REPORT;

     GENERIC
          TYPE G_TYPE IS PRIVATE;
     FUNCTION EQUAL_G (X : G_TYPE; Y : G_TYPE) RETURN BOOLEAN;

     FUNCTION EQUAL_G (X : G_TYPE; Y : G_TYPE) RETURN BOOLEAN IS
     BEGIN
          IF (IDENT_INT(3) = 3) AND (X = Y) THEN
               RETURN TRUE;
          ELSE
               RETURN FALSE;
          END IF;
     END EQUAL_G;

BEGIN

     TEST("C48009G","FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                    "APPROPRIATE - CONSTRAINED ACCESS TYPE");

     DECLARE

          TYPE INT IS RANGE 1 .. 5;

          TYPE UR(A : INT) IS
               RECORD
                    B : INTEGER;
               END RECORD;
          TYPE UA IS ARRAY(INT RANGE <>) OF INTEGER;

          PACKAGE P IS
               TYPE UP(A, B : INT) IS PRIVATE;
               TYPE UL(A, B : INT) IS LIMITED PRIVATE;
               CONS_UP : CONSTANT UP;
          PRIVATE
               TYPE UP(A, B : INT) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;
               TYPE UL(A, B : INT) IS
                    RECORD
                         C : INTEGER;
                    END RECORD;
               CONS_UP : CONSTANT UP := (2, 2, (IDENT_INT(3)));
          END P;

          TYPE A_UR IS ACCESS UR;
          TYPE A_UA IS ACCESS UA;
          TYPE A_UP IS ACCESS P.UP;
          TYPE A_UL IS ACCESS P.UL;

          SUBTYPE CA_UR IS A_UR(2);
          SUBTYPE CA_UA IS A_UA(2 .. 3);
          SUBTYPE CA_UP IS A_UP(3, 2);
          SUBTYPE CA_UL IS A_UL(2, 4);

          TYPE A_CA_UR IS ACCESS CA_UR;
          TYPE A_CA_UA IS ACCESS CA_UA;
          TYPE A_CA_UP IS ACCESS CA_UP;
          TYPE A_CA_UL IS ACCESS CA_UL;

          V_A_CA_UR : A_CA_UR;
          V_A_CA_UA : A_CA_UA;
          V_A_CA_UP : A_CA_UP;
          V_A_CA_UL : A_CA_UL;

          FUNCTION EQUAL IS NEW EQUAL_G(A_CA_UR);
          FUNCTION EQUAL IS NEW EQUAL_G(A_CA_UA);
          FUNCTION EQUAL IS NEW EQUAL_G(A_CA_UP);
          FUNCTION EQUAL IS NEW EQUAL_G(A_CA_UL);

     BEGIN

          BEGIN
               V_A_CA_UR := NEW CA_UR'(NEW UR'(1,(IDENT_INT(2))));

               IF EQUAL (V_A_CA_UR, V_A_CA_UR) THEN
                    FAILED ("NO EXCEPTION RAISED - UR");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UR");
          END;

          BEGIN
               V_A_CA_UA := NEW CA_UA'(NEW UA'(1 => 2,
                                               2 => IDENT_INT(3)));

               IF EQUAL (V_A_CA_UA, V_A_CA_UA) THEN
                    FAILED ("NO EXCEPTION RAISED - UA");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UA");
          END;

          BEGIN
               V_A_CA_UP := NEW CA_UP'(NEW P.UP'(P.CONS_UP));

               IF EQUAL (V_A_CA_UP, V_A_CA_UP) THEN
                    FAILED ("NO EXCEPTION RAISED - UP");
               END IF;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UP");
          END;

          BEGIN
               V_A_CA_UR := NEW CA_UR'(NULL);

               IF NOT EQUAL (V_A_CA_UR, V_A_CA_UR) THEN
                    COMMENT ("NO EXCEPTION RAISED - UR");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - UR");
          END;

          BEGIN
               V_A_CA_UA := NEW CA_UA'(NULL);

               IF NOT EQUAL (V_A_CA_UA, V_A_CA_UA) THEN
                    COMMENT ("NO EXCEPTION RAISED - UA");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - UA");
          END;

          BEGIN
               V_A_CA_UP := NEW CA_UP'(NULL);

               IF NOT EQUAL (V_A_CA_UP, V_A_CA_UP) THEN
                    COMMENT ("NO EXCEPTION RAISED - UP");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - UP");
          END;

          BEGIN
               V_A_CA_UL := NEW CA_UL'(NULL);

               IF NOT EQUAL (V_A_CA_UL, V_A_CA_UL) THEN
                    COMMENT ("NO EXCEPTION RAISED - UL");
               END IF;

          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED - UL");
          END;

     END;

     RESULT;

END C48009G;
