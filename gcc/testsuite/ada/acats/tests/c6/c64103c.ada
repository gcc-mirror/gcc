-- C64103C.ADA

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
-- CHECK THAT THE APPROPRIATE EXCEPTION IS RAISED FOR TYPE CONVERSIONS
-- ON IN OUT ARRAY PARAMETERS.  IN PARTICULAR:
--   (A) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL WHEN THE ACTUAL
--       COMPONENT'S CONSTRAINTS DIFFER FROM THE FORMAL COMPONENT'S
--       CONSTRAINTS.
--   (B) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL WHEN CONVERSION TO
--       AN UNCONSTRAINED ARRAY TYPE CAUSES AN ACTUAL INDEX BOUND TO LIE
--       OUTSIDE OF A FORMAL INDEX SUBTYPE FOR A NON-NULL DIMENSION (SEE
--       AI-00313 FOR MULTIDIMENSIONAL CASE)
--   (C) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL FOR CONVERSION TO A
--       CONSTRAINED ARRAY TYPE WHEN THE NUMBER OF COMPONENTS PER
--       DIMENSION OF THE ACTUAL DIFFERS FROM THAT OF THE FORMAL.
--   (D) CONSTRAINT_ERROR IS RAISED BEFORE THE CALL WHEN CONVERSION TO AN
--       UNCONSTRAINED ARRAY TYPE CAUSES AN ACTUAL INDEX BOUND TO LIE
--       OUTSIDE OF THE BASE INDEX TYPE OF THE FORMAL.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- CPP 07/19/84
-- JBG 06/05/85
-- EG  10/29/85  FIX NUMERIC_ERROR/CONSTRAINT_ERROR ACCORDING TO
--               AI-00387.
-- MRM 03/30/93  REMOVE NUMERIC_ERROR FOR 9X COMPATIBILITY
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM;
WITH REPORT;  USE REPORT;
PROCEDURE C64103C IS

     BEGIN
     TEST ("C64103C", "CHECK THAT APPROPRIATE EXCEPTION IS RAISED ON " &
           "TYPE CONVERSIONS OF IN OUT ARRAY PARAMETERS");

     -----------------------------------------------

     DECLARE   -- (A)
     BEGIN     -- (A)

          DECLARE
               TYPE SUBINT IS RANGE 0..8;
               TYPE ARRAY_TYPE IS ARRAY (SUBINT RANGE <>) OF BOOLEAN;
               A0 : ARRAY_TYPE (0..3) := (0..3 => TRUE);

               PROCEDURE P2 (X : IN OUT ARRAY_TYPE) IS
               BEGIN
                    NULL;
               END P2;
          BEGIN
               P2 (ARRAY_TYPE (A0));                  -- OK.
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED -P2 (A)");
          END;

     END; -- (A)

     -----------------------------------------------

     DECLARE   -- (B1) NON-NULL ACTUAL PARAMETER

          TYPE SUBINT IS RANGE 0..8;
          TYPE ARRAY_TYPE IS ARRAY (SUBINT RANGE <>) OF BOOLEAN;
          TYPE AR1 IS ARRAY (INTEGER RANGE <>) OF BOOLEAN;
          A1 : AR1 (-1..7) := (-1..7 => TRUE);
          A2 : AR1 (1..9) := (1..9 => TRUE);

          PROCEDURE P1 (X : IN OUT ARRAY_TYPE) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P1 (B)");
          END P1;

     BEGIN     -- (B1)

          BEGIN
               COMMENT ("CALL TO P1 (B1) ON A1");
               P1 (ARRAY_TYPE (A1));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B1)");
          END;

          BEGIN
               COMMENT ("CALL TO P1 (B1) ON A2");
               P1 (ARRAY_TYPE (A2));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B1)");
          END;

     END; -- (B1)

     DECLARE   -- (B2) NULL ACTUAL PARAMETER; MULTIDIMENSIONAL

          TYPE SUBINT IS RANGE 0..8;
          TYPE ARRAY_TYPE IS ARRAY (SUBINT RANGE <>,
                                    SUBINT RANGE <>) OF BOOLEAN;
          TYPE AR1 IS ARRAY (INTEGER RANGE <>,
                             INTEGER RANGE <>)OF BOOLEAN;
          A1 : AR1 (IDENT_INT(-1)..7, 5..4) :=
                                           (OTHERS => (OTHERS => TRUE));
          A2 : AR1 (5..4, 1..IDENT_INT(9)) :=
                                           (OTHERS => (OTHERS => TRUE));
          PROCEDURE P1 (X : IN OUT ARRAY_TYPE) IS
          BEGIN
               FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P1 (B)");
          END P1;

     BEGIN     -- (B2)

          BEGIN
               COMMENT ("CALL TO P1 (B2) ON A1");
               P1 (ARRAY_TYPE (A1));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B2)");
          END;

          BEGIN
               COMMENT ("CALL TO P1 (B2) ON A2");
               P1 (ARRAY_TYPE (A2));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (B2)");
          END;

     END; -- (B2)

     -----------------------------------------------

     BEGIN     -- (C)

          DECLARE
               TYPE INDEX1 IS RANGE 1..3;
               TYPE INDEX2 IS RANGE 1..4;
               TYPE AR_TYPE IS ARRAY (INDEX1, INDEX2) OF BOOLEAN;
               A0 : AR_TYPE := (1..3 => (1..4 => FALSE));

               TYPE I1 IS RANGE 1..4;
               TYPE I2 IS RANGE 1..3;
               TYPE ARRAY_TYPE IS ARRAY (I1, I2) OF BOOLEAN;

               PROCEDURE P1 (X : IN OUT ARRAY_TYPE) IS
               BEGIN
                    FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P1 (C)");
               END P1;
          BEGIN
               P1 (ARRAY_TYPE (A0));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED -P1 (C)");
          END;

     END; -- (C)

     -----------------------------------------------

     DECLARE   -- (D)
     BEGIN     -- (D)

          DECLARE
               TYPE SM_INT IS RANGE 0..2;
               TYPE LG IS RANGE 0 .. SYSTEM.MAX_INT;
               SUBTYPE LG_INT IS LG RANGE SYSTEM.MAX_INT - 3 ..
                                          SYSTEM.MAX_INT;
               TYPE AR_SMALL IS ARRAY (SM_INT RANGE <>) OF BOOLEAN;
               TYPE AR_LARGE IS ARRAY (LG_INT RANGE <>) OF BOOLEAN;
               A0 : AR_LARGE (SYSTEM.MAX_INT - 2..SYSTEM.MAX_INT) :=
                    (SYSTEM.MAX_INT - 2..SYSTEM.MAX_INT => TRUE);

               PROCEDURE P1 (X : IN OUT AR_SMALL) IS
               BEGIN
                    FAILED ("EXCEPTION NOT RAISED BEFORE CALL -P1 (D)");
               END P1;
          BEGIN
               IF LG (SM_INT'BASE'LAST) < LG_INT'BASE'LAST THEN
                    P1 (AR_SMALL (A0));
               ELSE
                    COMMENT ("NOT APPLICABLE -P1 (D)");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    COMMENT ("CONSTRAINT_ERROR RAISED - P1 (D)");
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - P1 (D)");
          END;

     END; -- (D)

     -----------------------------------------------

     RESULT;

END C64103C;
