-- C34007D.ADA

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
--     ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS PART 1 OF 2 TESTS
--     WHICH COVER THE OBJECTIVE.  THE SECOND PART IS IN TEST C34007V.

-- HISTORY:
--     JRK 09/25/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     BCB 04/12/90  SPLIT ORIGINAL TEST INTO C34007D.ADA AND
--                   C34007V.ADA.  PUT CHECK FOR 'STORAGE_SIZE IN
--                   EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34007D IS

     SUBTYPE COMPONENT IS INTEGER;

     TYPE DESIGNATED IS ARRAY (NATURAL RANGE <>) OF COMPONENT;

     SUBTYPE SUBDESIGNATED IS DESIGNATED (IDENT_INT (5) ..
                                          IDENT_INT (7));

     PACKAGE PKG IS

          TYPE PARENT IS ACCESS DESIGNATED;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_INT (5) .. IDENT_INT (7));

     X : T         := NEW SUBDESIGNATED'(OTHERS => 2);
     K : INTEGER   := X'SIZE;
     Y : T         := NEW SUBDESIGNATED'(1, 2, 3);
     W : PARENT    := NEW SUBDESIGNATED'(OTHERS => 2);
     C : COMPONENT := 1;
     N : CONSTANT  := 1;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          NULL;
     END A;

     FUNCTION V RETURN T IS
     BEGIN
          RETURN NEW SUBDESIGNATED'(OTHERS => C);
     END V;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF X = NULL OR ELSE
             EQUAL (X'LENGTH, X'LENGTH) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN NEW SUBDESIGNATED;
     END IDENT;

BEGIN
     TEST ("C34007D", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
                      "ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS " &
                      "PART 1 OF 2 TESTS WHICH COVER THE OBJECTIVE.  " &
                      "THE SECOND PART IS IN TEST C34007V");

     IF Y = NULL OR ELSE Y.ALL /= (1, 2, 3) THEN
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
          W := NEW SUBDESIGNATED'(1, 2, 3);
     END IF;
     X := T (W);
     IF X = NULL OR ELSE X = Y OR ELSE X.ALL /= (1, 2, 3) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     X := IDENT (Y);
     W := PARENT (X);
     IF W = NULL OR ELSE W.ALL /= (1, 2, 3) OR ELSE T (W) /= Y THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 1");
     END IF;

     IF IDENT (NULL) /= NULL OR X = NULL THEN
          FAILED ("INCORRECT NULL");
     END IF;

     X := IDENT (NEW SUBDESIGNATED'(1, 2, 3));
     IF (X = NULL OR ELSE X = Y OR ELSE X.ALL /= (1, 2, 3)) OR
        X = NEW DESIGNATED'(1, 2) THEN
          FAILED ("INCORRECT ALLOCATOR");
     END IF;

     X := IDENT (NULL);
     BEGIN
          IF X.ALL = (0, 0, 0) THEN
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
     X (IDENT_INT (7)) := 4;
     IF X /= Y OR Y.ALL /= (1, 2, 4) THEN
          FAILED ("INCORRECT INDEX (ASSIGNMENT)");
     END IF;

     Y.ALL := (1, 2, 3);
     X := IDENT (Y);
     X (IDENT_INT (5) .. IDENT_INT (6)) := (4, 5);
     IF X /= Y OR Y.ALL /= (4, 5, 3) THEN
          FAILED ("INCORRECT SLICE (ASSIGNMENT)");
     END IF;

     A (X'ADDRESS);

     IF X'FIRST /= 5 THEN
          FAILED ("INCORRECT OBJECT'FIRST");
     END IF;

     IF V'FIRST /= 5 THEN
          FAILED ("INCORRECT VALUE'FIRST");
     END IF;

     IF X'FIRST (N) /= 5 THEN
          FAILED ("INCORRECT OBJECT'FIRST (N)");
     END IF;

     IF V'FIRST (N) /= 5 THEN
          FAILED ("INCORRECT VALUE'FIRST (N)");
     END IF;

     IF X'LAST /= 7 THEN
          FAILED ("INCORRECT OBJECT'LAST");
     END IF;

     IF V'LAST /= 7 THEN
          FAILED ("INCORRECT VALUE'LAST");
     END IF;

     IF X'LAST (N) /= 7 THEN
          FAILED ("INCORRECT OBJECT'LAST (N)");
     END IF;

     IF V'LAST (N) /= 7 THEN
          FAILED ("INCORRECT VALUE'LAST (N)");
     END IF;

     IF X'LENGTH /= 3 THEN
          FAILED ("INCORRECT OBJECT'LENGTH");
     END IF;

     IF V'LENGTH /= 3 THEN
          FAILED ("INCORRECT VALUE'LENGTH");
     END IF;

     IF X'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT OBJECT'LENGTH (N)");
     END IF;

     IF V'LENGTH (N) /= 3 THEN
          FAILED ("INCORRECT VALUE'LENGTH (N)");
     END IF;

     DECLARE
          Y : DESIGNATED (X'RANGE);
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT OBJECT'RANGE");
          END IF;
     END;

     DECLARE
          Y : DESIGNATED (V'RANGE);
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT VALUE'RANGE");
          END IF;
     END;

     DECLARE
          Y : DESIGNATED (X'RANGE (N));
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT OBJECT'RANGE (N)");
          END IF;
     END;

     DECLARE
          Y : DESIGNATED (V'RANGE (N));
     BEGIN
          IF Y'FIRST /= 5 OR Y'LAST /= 7 THEN
               FAILED ("INCORRECT VALUE'RANGE (N)");
          END IF;
     END;

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
END C34007D;
