-- C34007G.ADA

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
--     CHECK THAT THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--     (IMPLICITLY) FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS A
--     MULTI-DIMENSIONAL ARRAY TYPE.

-- HISTORY:
--     JRK 09/25/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34007G IS

     SUBTYPE COMPONENT IS INTEGER;

     TYPE DESIGNATED IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF
                              COMPONENT;

     SUBTYPE SUBDESIGNATED IS DESIGNATED
                                   (IDENT_INT (4) .. IDENT_INT (5),
                                    IDENT_INT (6) .. IDENT_INT (8));

     PACKAGE PKG IS

          TYPE PARENT IS ACCESS DESIGNATED;

          FUNCTION CREATE ( F1, L1 : NATURAL;
                            F2, L2 : NATURAL;
                            C      : COMPONENT;
                            DUMMY  : PARENT   -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_INT (4) .. IDENT_INT (5),
                           IDENT_INT (6) .. IDENT_INT (8));

     X : T         := NEW SUBDESIGNATED'(OTHERS => (OTHERS => 2));
     Y : T         := NEW SUBDESIGNATED'((1, 2, 3), (4, 5, 6));
     W : PARENT    := NEW SUBDESIGNATED'(OTHERS => (OTHERS => 2));
     C : COMPONENT := 1;
     N : CONSTANT  := 2;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          NULL;
     END A;

     FUNCTION V RETURN T IS
     BEGIN
          RETURN NEW SUBDESIGNATED'(OTHERS => (OTHERS => C));
     END V;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( F1, L1 : NATURAL;
               F2, L2 : NATURAL;
               C      : COMPONENT;
               DUMMY  : PARENT
             ) RETURN PARENT
          IS
               A : PARENT    := NEW DESIGNATED (F1 .. L1, F2 .. L2);
               B : COMPONENT := C;
          BEGIN
               FOR I IN F1 .. L1 LOOP
                    FOR J IN F2 .. L2 LOOP
                         A (I, J) := B;
                         B := B + 1;
                    END LOOP;
               END LOOP;
               RETURN A;
          END CREATE;

     END PKG;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF X = NULL OR ELSE
             EQUAL (X'LENGTH, X'LENGTH) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN NEW SUBDESIGNATED;
     END IDENT;

BEGIN
     TEST ("C34007G", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
                      "MULTI-DIMENSIONAL ARRAY TYPE");

     IF Y = NULL OR ELSE Y.ALL /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT INITIALIZATION");
     END IF;

     X := IDENT (Y);
     IF X /= Y THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= Y THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= Y THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := NEW SUBDESIGNATED'((1, 2, 3), (4, 5, 6));
     END IF;
     X := T (W);
     IF X = NULL OR ELSE X = Y OR ELSE
        X.ALL /= ((1, 2, 3), (4, 5, 6)) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     X := IDENT (Y);
     W := PARENT (X);
     IF W = NULL OR ELSE W.ALL /= ((1, 2, 3), (4, 5, 6)) OR ELSE
        T (W) /= Y THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 1");
     END IF;

     W := PARENT (CREATE (6, 9, 2, 3, 4, X));
     IF W = NULL OR ELSE
        W.ALL /= ((4, 5), (6, 7), (8, 9), (10, 11)) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 2");
     END IF;

     IF IDENT (NULL) /= NULL OR X = NULL THEN
          FAILED ("INCORRECT NULL");
     END IF;

     X := IDENT (NEW SUBDESIGNATED'((1, 2, 3), (4, 5, 6)));
     IF (X = NULL OR ELSE X = Y OR ELSE
         X.ALL /= ((1, 2, 3), (4, 5, 6))) OR
        X = NEW DESIGNATED'((1, 2), (3, 4), (5, 6)) THEN
          FAILED ("INCORRECT ALLOCATOR");
     END IF;

     X := IDENT (Y);
     IF X.ALL /= ((1, 2, 3), (4, 5, 6)) OR
        CREATE (6, 9, 2, 3, 4, X) . ALL /=
        ((4, 5), (6, 7), (8, 9), (10, 11)) THEN
          FAILED ("INCORRECT .ALL (VALUE)");
     END IF;

     X.ALL := ((10, 11, 12), (13, 14, 15));
     IF X /= Y OR Y.ALL /= ((10, 11, 12), (13, 14, 15)) THEN
          FAILED ("INCORRECT .ALL (ASSIGNMENT)");
     END IF;

     Y.ALL := ((1, 2, 3), (4, 5, 6));
     BEGIN
          CREATE (6, 9, 2, 3, 4, X) . ALL :=
               ((20, 21), (22, 23), (24, 25), (26, 27));
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR .ALL (ASSIGNMENT)");
     END;

     X := IDENT (NULL);
     BEGIN
          IF X.ALL = ((0, 0, 0), (0, 0, 0)) THEN
               FAILED ("NO EXCEPTION FOR NULL.ALL - 1");
          ELSE FAILED ("NO EXCEPTION FOR NULL.ALL - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION FOR NULL.ALL");
     END;

     X := IDENT (Y);
     IF X (IDENT_INT (4), IDENT_INT (6)) /= 1 OR
        CREATE (6, 9, 2, 3, 4, X) (9, 3) /= 11 THEN
          FAILED ("INCORRECT INDEX (VALUE)");
     END IF;

     X (IDENT_INT (5), IDENT_INT (8)) := 7;
     IF X /= Y OR Y.ALL /= ((1, 2, 3), (4, 5, 7)) THEN
          FAILED ("INCORRECT INDEX (ASSIGNMENT)");
     END IF;

     Y.ALL := ((1, 2, 3), (4, 5, 6));
     X := IDENT (Y);
     BEGIN
          CREATE (6, 9, 2, 3, 4, X) (6, 2) := 15;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR INDEX (ASSIGNMENT)");
     END;

     IF X = NULL OR X = NEW SUBDESIGNATED OR NOT (X = Y) OR
        X = CREATE (6, 9, 2, 3, 4, X) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= Y OR NOT (X /= NULL) OR
        NOT (X /= CREATE (7, 9, 2, 4, 1, X)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) OR CREATE (2, 3, 4, 5, 1, X) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (CREATE (7, 9, 2, 4, 1, X) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     A (X'ADDRESS);

     IF X'FIRST /= 4 THEN
          FAILED ("INCORRECT OBJECT'FIRST");
     END IF;

     IF V'FIRST /= 4 THEN
          FAILED ("INCORRECT VALUE'FIRST");
     END IF;

     IF X'FIRST (N) /= 6 THEN
          FAILED ("INCORRECT OBJECT'FIRST (N)");
     END IF;

     IF V'FIRST (N) /= 6 THEN
          FAILED ("INCORRECT VALUE'FIRST (N)");
     END IF;

     IF X'LAST /= 5 THEN
          FAILED ("INCORRECT OBJECT'LAST");
     END IF;

     IF V'LAST /= 5 THEN
          FAILED ("INCORRECT VALUE'LAST");
     END IF;

     IF X'LAST (N) /= 8 THEN
          FAILED ("INCORRECT OBJECT'LAST (N)");
     END IF;

     IF V'LAST (N) /= 8 THEN
          FAILED ("INCORRECT VALUE'LAST (N)");
     END IF;

     IF X'LENGTH /= 2 THEN
          FAILED ("INCORRECT OBJECT'LENGTH");
     END IF;

     IF V'LENGTH /= 2 THEN
          FAILED ("INCORRECT VALUE'LENGTH");
     END IF;

     IF X'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT OBJECT'LENGTH (N)");
     END IF;

     IF V'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT VALUE'LENGTH (N)");
     END IF;

     DECLARE
          Y : DESIGNATED (X'RANGE, 1 .. 3);
     BEGIN
          IF Y'FIRST /= 4 OR Y'LAST /= 5 THEN
               FAILED ("INCORRECT OBJECT'RANGE");
          END IF;
     END;

     DECLARE
          Y : DESIGNATED (V'RANGE, 1 .. 3);
     BEGIN
          IF Y'FIRST /= 4 OR Y'LAST /= 5 THEN
               FAILED ("INCORRECT VALUE'RANGE");
          END IF;
     END;

     DECLARE
          Y : DESIGNATED (1 .. 2, X'RANGE (N));
     BEGIN
          IF Y'FIRST (N) /= 6 OR Y'LAST (N) /= 8 THEN
               FAILED ("INCORRECT OBJECT'RANGE (N)");
          END IF;
     END;

     DECLARE
          Y : DESIGNATED (1 .. 2, V'RANGE (N));
     BEGIN
          IF Y'FIRST (N) /= 6 OR Y'LAST (N) /= 8 THEN
               FAILED ("INCORRECT VALUE'RANGE (N)");
          END IF;
     END;

     IF T'SIZE < 1 THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE < T'SIZE THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     BEGIN
          IF T'STORAGE_SIZE /= PARENT'STORAGE_SIZE THEN
               FAILED ("COLLECTION SIZE OF DERIVED TYPE IS NOT " &
                       "EQUAL TO COLLECTION SIZE OF PARENT TYPE");
          END IF;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               COMMENT ("PROGRAM_ERROR RAISED FOR " &
                        "UNDEFINED STORAGE_SIZE (AI-00608)");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED");
     END;

     RESULT;
END C34007G;
