-- C43004A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF A VALUE FOR A
--     NON-DISCRIMINANT SCALAR COMPONENT OF AN AGGREGATE IS NOT
--     WITHIN THE RANGE OF THE COMPONENT'S SUBTYPE.

-- HISTORY:
--     BCB 01/22/88  CREATED ORIGINAL TEST.
--     RJW 06/27/90  CORRECTED CONSTRAINTS OF TYPE DFIX.
--     LDC 09/25/90  ADDED A BLOCK IN THE EXCEPTION HANDLER SO IT CAN
--                   NOT OPTIMIZE IT AWAY,  ALSO INITIALIZED EACH
--                   OBJECT TO VALID DATA BEFORE DOING THE INVALID,
--                   MADE 'IDENT_XXX' FUNCTIONS SO THE COMPILER CAN
--                   NOT JUST EVALUATE THE ASSIGNMENT AND PUT IN CODE
--                   FOR A CONSTRAINT ERROR IN IS PLACE.
--     JRL 06/07/96  Changed value in aggregate in subtest 4 to value
--                   guaranteed to be in the base range of the type FIX.
--                   Corrected typo.

WITH REPORT; USE REPORT;

PROCEDURE C43004A IS

     TYPE INT IS RANGE 1 .. 8;
     SUBTYPE SINT IS INT RANGE 2 .. 7;

     TYPE ENUM IS (VINCE, JOHN, TOM, PHIL, ROSA, JODIE, BRIAN, DAVE);
     SUBTYPE SENUM IS ENUM RANGE JOHN .. BRIAN;

     TYPE FL IS DIGITS 5 RANGE 0.0 .. 10.0;
     SUBTYPE SFL IS FL RANGE 1.0 .. 9.0;

     TYPE FIX IS DELTA 0.25 RANGE 0.0 .. 8.0;
     SUBTYPE SFIX IS FIX RANGE 1.0 .. 7.0;

     TYPE DINT IS NEW INTEGER RANGE 1 .. 8;
     SUBTYPE SDINT IS DINT RANGE 2 .. 7;

     TYPE DENUM IS NEW ENUM RANGE VINCE .. DAVE;
     SUBTYPE SDENUM IS DENUM RANGE JOHN .. BRIAN;

     TYPE DFL IS NEW FLOAT RANGE 0.0 .. 10.0;
     SUBTYPE SDFL IS DFL RANGE 1.0 .. 9.0;

     TYPE DFIX IS NEW FIX RANGE 0.5 .. 7.5;
     SUBTYPE SDFIX IS DFIX RANGE 1.0 .. 7.0;

     TYPE REC1 IS RECORD
           E1, E2, E3, E4, E5 : SENUM;
     END RECORD;

     TYPE REC2 IS RECORD
           E1, E2, E3, E4, E5 : SFIX;
     END RECORD;

     TYPE REC3 IS RECORD
           E1, E2, E3, E4, E5 : SDENUM;
     END RECORD;

     TYPE REC4 IS RECORD
           E1, E2, E3, E4, E5 : SDFIX;
     END RECORD;

     ARRAY_OBJ : ARRAY(1..2) OF INTEGER;

     A  : ARRAY(1..5) OF SINT;
     B  : REC1;
     C  : ARRAY(1..5) OF SFL;
     D  : REC2;
     E  : ARRAY(1..5) OF SDINT;
     F  : REC3;
     G  : ARRAY(1..5) OF SDFL;
     H  : REC4;

     GENERIC
          TYPE GENERAL_PURPOSE IS PRIVATE;
     FUNCTION GENEQUAL(ONE, TWO : GENERAL_PURPOSE) RETURN BOOLEAN;

     FUNCTION GENEQUAL(ONE, TWO : GENERAL_PURPOSE) RETURN BOOLEAN IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN ONE = TWO;
          ELSE
               RETURN ONE /= TWO;
          END IF;
     END GENEQUAL;

     FUNCTION EQUAL IS NEW GENEQUAL(SENUM);
     FUNCTION EQUAL IS NEW GENEQUAL(SFL);
     FUNCTION EQUAL IS NEW GENEQUAL(SFIX);
     FUNCTION EQUAL IS NEW GENEQUAL(SDENUM);
     FUNCTION EQUAL IS NEW GENEQUAL(SDFL);
     FUNCTION EQUAL IS NEW GENEQUAL(SDFIX);

     GENERIC
          TYPE GENERAL_PURPOSE IS PRIVATE;
          WITH FUNCTION EQUAL_GENERAL(ONE, TWO : GENERAL_PURPOSE) 
                   RETURN BOOLEAN;
     FUNCTION GEN_IDENT (X : GENERAL_PURPOSE) RETURN GENERAL_PURPOSE;
     FUNCTION GEN_IDENT (X : GENERAL_PURPOSE) RETURN GENERAL_PURPOSE IS
          BEGIN
          IF EQUAL_GENERAL (X, X) THEN  -- ALWAYS EQUAL.
               RETURN X;                -- ALWAYS EXECUTED.
          END IF;
                                        -- NEVER EXECUTED.
          RETURN X;
     END GEN_IDENT;

     FUNCTION IDENT_FL    IS NEW GEN_IDENT(FL, EQUAL);
     FUNCTION IDENT_FIX   IS NEW GEN_IDENT(FIX, EQUAL);
     FUNCTION IDENT_DFL   IS NEW GEN_IDENT(DFL, EQUAL);
     FUNCTION IDENT_DFIX  IS NEW GEN_IDENT(DFIX, EQUAL);

BEGIN
     TEST ("C43004A", "CHECK THAT CONSTRAINT_ERROR IS RAISED IF A " &
                      "VALUE FOR A NON-DISCRIMINANT SCALAR COMPONENT " &
                      "OF AN AGGREGATE IS NOT WITHIN THE RANGE OF " &
                      "THE COMPONENT'S SUBTYPE");

     ARRAY_OBJ := (1, 2);

     BEGIN
          A  := (2,3,4,5,6);            -- OK

          IF EQUAL (INTEGER (A(IDENT_INT(1))),
                    INTEGER (A(IDENT_INT(2)))) THEN
               COMMENT ("DON'T OPTIMIZE A");
          END IF;

          A := (SINT(IDENT_INT(1)),2,3,4,7);
                                       -- CONSTRAINT_ERROR BY AGGREGATE
                                       -- WITH INTEGER COMPONENTS.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 1");
          IF EQUAL (INTEGER (A(IDENT_INT(1))),
                    INTEGER (A(IDENT_INT(1)))) THEN
               COMMENT ("DON'T OPTIMIZE A");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 1");
     END;

     BEGIN
          B := (JOHN,TOM,PHIL,ROSA,JOHN);  -- OK

          IF EQUAL (B.E1, B.E2) THEN
               COMMENT ("DON'T OPTIMIZE B");
          END IF;

          B := (ENUM'VAL(IDENT_INT(ENUM'POS(DAVE))), TOM, PHIL,
                ROSA, JODIE);
                                       -- CONSTRAINT_ERROR BY AGGREGATE
                                       -- WITH COMPONENTS OF AN
                                       -- ENUMERATION TYPE.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 2");
          IF NOT EQUAL (B.E1, B.E1) THEN
               COMMENT ("DON'T OPTIMIZE B");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 2");
     END;
     BEGIN
          C := (2.0,3.0,4.0,5.0,6.0);  -- OK
          IF EQUAL (C(IDENT_INT(1)), C(IDENT_INT(2))) THEN
               COMMENT ("DON'T OPTIMIZE C");
          END IF;

          C := (IDENT_FL(1.0),2.0,3.0,4.0,IDENT_FL(10.0));
                                      -- CONSTRAINT_ERROR BY AGGREGATE
                                      -- WITH FLOATING POINT COMPONENTS.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 3");
          IF NOT EQUAL (C(IDENT_INT(1)), C(IDENT_INT(1))) THEN
               COMMENT ("DON'T OPTIMIZE C");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 3");
     END;

     BEGIN
          D := (2.2,3.3,4.4,5.5,6.6); -- OK
          IF EQUAL (D.E1, D.E5) THEN
               COMMENT ("DON'T OPTIMIZE D");
          END IF;

          D := (IDENT_FIX(1.0),2.1,3.3,4.4,IDENT_FIX(7.75));
                                       -- CONSTRAINT_ERROR BY AGGREGATE
                                       -- WITH FIXED POINT COMPONENTS.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 4");
          IF NOT EQUAL (D.E5, D.E5) THEN
               COMMENT ("DON'T OPTIMIZE D");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 4");
     END;

     BEGIN
          E := (2,3,4,5,6); -- OK
          IF EQUAL (INTEGER (E(IDENT_INT(1))),
                    INTEGER (E(IDENT_INT(2)))) THEN
               COMMENT ("DON'T OPTIMIZE E");
          END IF;

          E := (SDINT(IDENT_INT(1)),2,3,4,7);
                                     -- CONSTRAINT_ERROR BY AGGREGATE
                                     -- WITH DERIVED INTEGER COMPONENTS.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 5");
          IF NOT EQUAL (INTEGER (E(IDENT_INT(1))),
                        INTEGER (E(IDENT_INT(1)))) THEN
               COMMENT ("DON'T OPTIMIZE E");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 5");
     END;

     BEGIN
          F := (JOHN,TOM,PHIL,ROSA,JOHN);  -- OK
          IF EQUAL (F.E1, F.E2) THEN
               COMMENT ("DON'T OPTIMIZE F");
          END IF;

          F := (DENUM'VAL(IDENT_INT(DENUM'POS(VINCE))), TOM, PHIL,
                ROSA, JODIE);
                                       -- CONSTRAINT_ERROR BY AGGREGATE
                                       -- WITH COMPONENTS OF A DERIVED
                                       -- ENUMERATION TYPE.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 6");
          IF NOT EQUAL (F.E1, F.E1) THEN
               COMMENT ("DON'T OPTIMIZE F");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 6");
     END;

     BEGIN
          G := (2.0,3.0,4.0,5.0,6.0); -- OK
          IF EQUAL (G(IDENT_INT(1)), G(IDENT_INT(2))) THEN
               COMMENT ("DON'T OPTIMIZE G");
          END IF;

          G := (IDENT_DFL(1.0),2.0,3.0,4.0,IDENT_DFL(10.0));
                                       -- CONSTRAINT_ERROR BY AGGREGATE
                                       -- WITH DERIVED FLOATING POINT
                                       -- COMPONENTS.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 7");
          IF NOT EQUAL (G(IDENT_INT(1)), G(IDENT_INT(1))) THEN
               COMMENT ("DON'T OPTIMIZE G");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 7");
     END;

     BEGIN
          H := (2.2,3.3,4.4,5.5,6.6); -- OK
          IF EQUAL (H.E1, H.E2) THEN
               COMMENT ("DON'T OPTIMIZE H");
          END IF;

          H := (IDENT_DFIX(2.0),2.5,3.5,4.3,IDENT_DFIX(7.4));
                                       -- CONSTRAINT_ERROR BY AGGREGATE
                                       -- WITH DERIVED FIXED POINT
                                       -- COMPONENTS.
          FAILED ("CONSTRAINT_ERROR WAS NOT RAISED - 8");
          IF EQUAL (H.E1, H.E5) THEN
               COMMENT ("DON'T OPTIMIZE H");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF EQUAL (ARRAY_OBJ(IDENT_INT(1)),
                         ARRAY_OBJ(IDENT_INT(2))) THEN
                    COMMENT ("DON'T OPTIMIZE EXCEPTION HANDLER");
               END IF;
          WHEN OTHERS =>
               FAILED ("AN EXCEPTION OTHER THAN CONSTRAINT_ERROR " &
                       "WAS RAISED - 8");
     END;


     RESULT;
END C43004A;
