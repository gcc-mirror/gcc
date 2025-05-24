-- C43207B.ADA

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
-- FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM (F..G => (H..I => J)),
-- CHECK THAT:

--     B) IF H..I IS A NULL RANGE, CONSTRAINT_ERROR IS RAISED IF
--        F..G IS NON-NULL AND F OR G DO NOT BELONG TO THE INDEX
--        SUBTYPE;

-- EG  01/18/84
-- BHS  7/13/84
-- JBG 12/6/84

WITH REPORT;

PROCEDURE C43207B IS

     USE REPORT;

BEGIN

     TEST("C43207B", "CHECK THAT THE EVALUATION OF A MULTI" &
                     "DIMENSIONAL AGGREGATE OF THE FORM "   &
                     "(F..G => (H..I = J)) IS PERFORMED "   &
                     "CORRECTLY");

     DECLARE

          TYPE CHOICE_INDEX IS (F, G, H, I, J);
          TYPE CHOICE_CNTR  IS ARRAY(CHOICE_INDEX) OF INTEGER;

          CNTR : CHOICE_CNTR := (CHOICE_INDEX => 0);

          SUBTYPE SINT IS INTEGER RANGE 1 .. 8;
          TYPE T0 IS ARRAY(SINT RANGE <>, SINT RANGE <>) OF INTEGER;
   
          FUNCTION CALC (A : CHOICE_INDEX; B : INTEGER)
                         RETURN INTEGER IS
          BEGIN
               CNTR(A) := CNTR(A) + 1;
               RETURN IDENT_INT(B);
          END CALC;

     BEGIN

CASE_B :  DECLARE
               PROCEDURE CHECK (A : T0; M : STRING) IS
               BEGIN
                    IF (A'FIRST(1) /= 1) OR (A'LAST(1) /= 9) OR
                       (A'FIRST(2) /= 6) OR (A'LAST(2) /= 5) THEN
                         FAILED("CASE B" & M & " : ARRAY NOT " &
                                "BOUNDED CORRECTLY");
                    END IF;
               END CHECK;
          BEGIN

     CASE_B1 : BEGIN
                    CHECK ((1 .. 9 => (6 .. 5 => 2)),"1");
                    FAILED ("CASE B1 : CONSTRAINT_ERROR NOT RAISED");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("CASE B1 : EXCEPTION RAISED");
               END CASE_B1;

     CASE_B2 : BEGIN
                    CHECK ((CALC(F,1) .. CALC(G,9) => (6 .. 5 => 2)),
                           "2");
                    FAILED ("CASE B2 : CONSTRAINT_ERROR NOT RAISED");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("CASE B2 : EXCEPTION RAISED");
               END CASE_B2;

     CASE_B3 : BEGIN
                    CHECK ((1 .. 9 => (CALC(H,6) .. CALC(I,5) => 2)),
                           "3");
                    FAILED ("CASE B3 : CONSTRAINT_ERROR NOT RAISED");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED("CASE B3 : EXCEPTION RAISED");
               END CASE_B3;

          END CASE_B;

     IF CNTR(F) /= 1 THEN
          FAILED ("CASE B2 : F WAS NOT EVALUATED " &
                  "ONCE.  F WAS EVALUATED"         & 
                  INTEGER'IMAGE(CNTR(F)) & " TIMES");
     END IF;
     IF CNTR(G) /= 1 THEN
          FAILED ("CASE B2 : G WAS NOT EVALUATED " &
                  "ONCE.  G WAS EVALUATED"         &
                  INTEGER'IMAGE(CNTR(G)) & " TIMES");
     END IF;

     IF CNTR(H) /= 0 AND CNTR(I) /= 0 THEN
          COMMENT ("CASE B3 : ALL CHOICES " &
                   "EVALUATED BEFORE CHECKING " &
                   "INDEX SUBTYPE");
     ELSIF CNTR(H) = 0 AND CNTR(I) = 0 THEN
          COMMENT ("CASE B3 : SUBTYPE CHECKS "&
                   "MADE AS CHOICES ARE EVALUATED");
     END IF;
                              
     IF CNTR(H) > 1 THEN
          FAILED("CASE B3 : H WAS NOT EVALUATED " &
                 "AT MOST ONCE. H WAS EVALUATED" &
                 INTEGER'IMAGE(CNTR(H)) & " TIMES");
     END IF;

     IF CNTR(I) > 1 THEN
          FAILED("CASE B3 : I WAS NOT EVALUATED " &
                 "AT MOST ONCE. I WAS EVALUATED" &
                 INTEGER'IMAGE(CNTR(I)) & " TIMES");
     END IF;

     END;

     RESULT;

END C43207B;
