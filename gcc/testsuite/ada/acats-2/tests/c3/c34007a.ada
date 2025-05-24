-- C34007A.ADA

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
--     (IMPLICITLY) FOR DERIVED ACCESS TYPES WHOSE DESIGNATED TYPE IS
--     NOT AN ARRAY TYPE, A TASK TYPE, A RECORD TYPE, OR A TYPE WITH
--     DISCRIMINANTS.

-- HISTORY:
--     JRK 09/24/86  CREATED ORIGINAL TEST.
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  REVISED TEST SO
--                   T'STORAGE_SIZE IS NOT REQUIRED TO BE > 1.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     BCB 03/07/90  PUT CHECK FOR 'STORAGE_SIZE IN EXCEPTION HANDLER.
--     THS 09/18/90  REMOVED DECLARATION OF B, MADE THE BODY OF
--                   PROCEDURE A NULL, AND DELETED ALL REFERENCES TO B.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34007A IS

     TYPE DESIGNATED IS RANGE -100 .. 100;

     SUBTYPE SUBDESIGNATED IS DESIGNATED RANGE
               DESIGNATED'VAL (IDENT_INT (-50)) ..
               DESIGNATED'VAL (IDENT_INT ( 50));

     TYPE PARENT IS ACCESS SUBDESIGNATED RANGE
               DESIGNATED'VAL (IDENT_INT (-30)) ..
               DESIGNATED'VAL (IDENT_INT ( 30));

     TYPE T IS NEW PARENT;

     X : T       := NEW DESIGNATED'(-30);
     K : INTEGER := X'SIZE;
     Y : T       := NEW DESIGNATED'( 30);
     W : PARENT  := NEW DESIGNATED'( 30);

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          NULL;
     END A;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF X = NULL OR ELSE
             EQUAL (DESIGNATED'POS (X.ALL), DESIGNATED'POS (X.ALL)) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN NEW DESIGNATED;
     END IDENT;

BEGIN
     TEST ("C34007A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS NOT AN " &
                      "ARRAY TYPE, A TASK TYPE, A RECORD TYPE, OR A " &
                      "TYPE WITH DISCRIMINANTS");

     IF Y = NULL OR ELSE Y.ALL /= 30 THEN
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
          W := NEW DESIGNATED'(-30);
     END IF;
     X := T (W);
     IF X = NULL OR ELSE X = Y OR ELSE X.ALL /= -30 THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     X := IDENT (Y);
     W := PARENT (X);
     IF W = NULL OR ELSE W.ALL /= 30 OR ELSE T (W) /= Y THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF IDENT (NULL) /= NULL OR X = NULL THEN
          FAILED ("INCORRECT NULL");
     END IF;

     X := IDENT (NEW DESIGNATED'(30));
     IF X = NULL OR ELSE X = Y OR ELSE X.ALL /= 30 THEN
          FAILED ("INCORRECT ALLOCATOR");
     END IF;

     X := IDENT (Y);
     IF X.ALL /= 30 THEN
          FAILED ("INCORRECT .ALL (VALUE)");
     END IF;

     X.ALL := DESIGNATED'VAL (IDENT_INT (10));
     IF X /= Y OR Y.ALL /= 10 THEN
          FAILED ("INCORRECT .ALL (ASSIGNMENT)");
     END IF;

     Y.ALL := 30;
     X := IDENT (NULL);
     BEGIN
          IF X.ALL = 0 THEN
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
     IF X = NULL OR X = NEW DESIGNATED OR NOT (X = Y) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= Y OR NOT (X /= NULL) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     A (X'ADDRESS);

     BEGIN
          IF T'STORAGE_SIZE /= PARENT'STORAGE_SIZE THEN
               FAILED ("COLLECTION SIZE OF DERIVED TYPE IS NOT " &
                       "EQUAL OF COLLECTION SIZE OF PARENT TYPE");
          END IF;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               COMMENT ("PROGRAM_ERROR RAISED FOR " &
                        "UNDEFINED STORAGE_SIZE (AI-00608)");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED");
     END;

     RESULT;
END C34007A;
