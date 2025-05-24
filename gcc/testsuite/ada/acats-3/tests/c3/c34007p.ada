-- C34007P.ADA

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
--     RECORD TYPE WITH DISCRIMINANTS.

-- HISTORY:
--     JRK 09/29/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34007P IS

     SUBTYPE COMPONENT IS INTEGER;

     SUBTYPE LENGTH IS NATURAL RANGE 0 .. 10;

     TYPE DESIGNATED (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
          RECORD
               I : INTEGER;
               CASE B IS
                    WHEN TRUE =>
                         S : STRING (1 .. L);
                         C : COMPONENT;
                    WHEN FALSE =>
                         F : FLOAT := 5.0;
               END CASE;
          END RECORD;

     SUBTYPE SUBDESIGNATED IS DESIGNATED (IDENT_BOOL (TRUE),
                                          IDENT_INT (3));

     PACKAGE PKG IS

          TYPE PARENT IS ACCESS DESIGNATED;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            C : COMPONENT;
                            F : FLOAT;
                            X : PARENT  -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     X : T       := NEW DESIGNATED'(TRUE, 3, 2, "AAA", 2);
     K : INTEGER := X'SIZE;
     Y : T       := NEW DESIGNATED'(TRUE, 3, 1, "ABC", 4);
     W : PARENT  := NEW DESIGNATED'(TRUE, 3, 2, "AAA", 2);
     C : COMPONENT := 1;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          NULL;
     END A;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               C : COMPONENT;
               F : FLOAT;
               X : PARENT
             ) RETURN PARENT
          IS
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN NEW DESIGNATED'(TRUE, L, I, S, C);
                    WHEN FALSE =>
                         RETURN NEW DESIGNATED'(FALSE, L, I, F);
               END CASE;
          END CREATE;

     END PKG;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF X = NULL OR ELSE EQUAL (X.I, X.I) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN NEW DESIGNATED'(TRUE, 3, -1, "---", -1);
     END IDENT;

BEGIN
     TEST ("C34007P", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
                      "RECORD TYPE WITH DISCRIMINANTS");

     IF Y = NULL OR ELSE Y.ALL /= (TRUE, 3, 1, "ABC", 4) THEN
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
          W := NEW DESIGNATED'(TRUE, 3, 1, "ABC", 4);
     END IF;
     X := T (W);
     IF X = NULL OR ELSE X = Y OR ELSE
        X.ALL /= (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     X := IDENT (Y);
     W := PARENT (X);
     IF W = NULL OR ELSE W.ALL /= (TRUE, 3, 1, "ABC", 4) OR ELSE
        T (W) /= Y THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 1");
     END IF;

     W := PARENT (CREATE (FALSE, 2, 3, "XX", 5, 6.0, X));
     IF W = NULL OR ELSE W.ALL /= (FALSE, 2, 3, 6.0) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 2");
     END IF;

     IF IDENT (NULL) /= NULL OR X = NULL THEN
          FAILED ("INCORRECT NULL");
     END IF;

     X := IDENT (NEW DESIGNATED'(TRUE, 3, 1, "ABC", 4));
     IF (X = NULL OR ELSE X = Y OR ELSE
         X.ALL /= (TRUE, 3, 1, "ABC", 4)) OR
        X = NEW DESIGNATED'(FALSE, 3, 1, 4.0) THEN
          FAILED ("INCORRECT ALLOCATOR");
     END IF;

     X := IDENT (Y);
     IF X.B /= TRUE OR X.L /= 3 OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . B /= FALSE OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . L /= 2 THEN
          FAILED ("INCORRECT SELECTION (DISCRIMINANT)");
     END IF;

     IF X.I /= 1 OR X.S /= "ABC" OR X.C /= 4 OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . I /= 3 OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . F /= 6.0 THEN
          FAILED ("INCORRECT SELECTION (VALUE)");
     END IF;

     X.I := IDENT_INT (7);
     X.S := IDENT_STR ("XYZ");
     X.C := IDENT_INT (9);
     IF X /= Y OR Y.ALL /= (TRUE, 3, 7, "XYZ", 9) THEN
          FAILED ("INCORRECT SELECTION (ASSIGNMENT)");
     END IF;

     Y.ALL := (TRUE, 3, 1, "ABC", 4);
     X := IDENT (Y);
     BEGIN
          CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . I := 10;
          CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . F := 10.0;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR SELECTION (ASSIGNMENT)");
     END;

     IF X.ALL /= (TRUE, 3, 1, "ABC", 4) OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . ALL /=
        (FALSE, 2, 3, 6.0) THEN
          FAILED ("INCORRECT .ALL (VALUE)");
     END IF;

     X.ALL := (TRUE, 3, 10, "ZZZ", 15);
     IF X /= Y OR Y.ALL /= (TRUE, 3, 10, "ZZZ", 15) THEN
          FAILED ("INCORRECT .ALL (ASSIGNMENT)");
     END IF;

     Y.ALL := (TRUE, 3, 1, "ABC", 4);
     BEGIN
          CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . ALL :=
               (FALSE, 2, 10, 15.0);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR .ALL (ASSIGNMENT)");
     END;

     X := IDENT (NULL);
     BEGIN
          IF X.ALL = (FALSE, 0, 0, 0.0) THEN
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
     IF X = NULL OR X = NEW SUBDESIGNATED OR NOT (X = Y) OR
        X = CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= Y OR NOT (X /= NULL) OR
        NOT (X /= CREATE (FALSE, 2, 3, "XX", 5, 6.0, X)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) OR CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR
        NOT (CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     A (X'ADDRESS);

     IF T'SIZE < 1 THEN
          FAILED ("INCORRECT TYPE'SIZE");
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
END C34007P;
