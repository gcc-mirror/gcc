-- C34007V.ADA

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
--     ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS PART 2 OF 2 TESTS
--     WHICH COVER THE OBJECTIVE.  THE FIRST PART IS IN TEST C34007D.

-- HISTORY:
--     BCB 04/12/90  CREATED ORIGINAL TEST FROM SPLIT OF C34007D.ADA.
--     THS 09/18/90  REMOVED DECLARATION OF B, DELETED PROCEDURE A,
--                   AND REMOVED ALL REFERENCES TO B.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34007V IS

     SUBTYPE COMPONENT IS INTEGER;

     TYPE DESIGNATED IS ARRAY (NATURAL RANGE <>) OF COMPONENT;

     SUBTYPE SUBDESIGNATED IS DESIGNATED (IDENT_INT (5) ..
                                          IDENT_INT (7));

     PACKAGE PKG IS

          TYPE PARENT IS ACCESS DESIGNATED;

          FUNCTION CREATE ( F, L  : NATURAL;
                            C     : COMPONENT;
                            DUMMY : PARENT   -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_INT (5) .. IDENT_INT (7));

     X : T         := NEW SUBDESIGNATED'(OTHERS => 2);
     K : INTEGER   := X'SIZE;
     Y : T         := NEW SUBDESIGNATED'(1, 2, 3);
     W : PARENT    := NEW SUBDESIGNATED'(OTHERS => 2);
     C : COMPONENT := 1;
     N : CONSTANT  := 1;

     FUNCTION V RETURN T IS
     BEGIN
          RETURN NEW SUBDESIGNATED'(OTHERS => C);
     END V;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( F, L  : NATURAL;
               C     : COMPONENT;
               DUMMY : PARENT
             ) RETURN PARENT
          IS
               A : PARENT    := NEW DESIGNATED (F .. L);
               B : COMPONENT := C;
          BEGIN
               FOR I IN F .. L LOOP
                    A (I) := B;
                    B := B + 1;
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
     TEST ("C34007V", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "ACCESS TYPES WHOSE DESIGNATED TYPE IS A " &
                      "ONE-DIMENSIONAL ARRAY TYPE.  THIS TEST IS " &
                      "PART 2 OF 2 TESTS WHICH COVER THE OBJECTIVE.  " &
                      "THE FIRST PART IS IN TEST C34007V");

     W := PARENT (CREATE (2, 3, 4, X));
     IF W = NULL OR ELSE W.ALL /= (4, 5) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT - 2");
     END IF;

     X := IDENT (Y);
     IF X.ALL /= (1, 2, 3) OR CREATE (2, 3, 4, X) . ALL /= (4, 5) THEN
          FAILED ("INCORRECT .ALL (VALUE)");
     END IF;

     X.ALL := (10, 11, 12);
     IF X /= Y OR Y.ALL /= (10, 11, 12) THEN
          FAILED ("INCORRECT .ALL (ASSIGNMENT)");
     END IF;

     Y.ALL := (1, 2, 3);
     BEGIN
          CREATE (2, 3, 4, X) . ALL := (10, 11);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR .ALL (ASSIGNMENT)");
     END;


     X := IDENT (Y);
     IF X (IDENT_INT (5)) /= 1 OR
        CREATE (2, 3, 4, X) (3) /= 5 THEN
          FAILED ("INCORRECT INDEX (VALUE)");
     END IF;

     Y.ALL := (1, 2, 3);
     X := IDENT (Y);
     BEGIN
          CREATE (2, 3, 4, X) (2) := 10;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR INDEX (ASSIGNMENT)");
     END;

     IF X (IDENT_INT (6) .. IDENT_INT (7)) /= (2, 3) OR
        CREATE (1, 4, 4, X) (1 .. 3) /= (4, 5, 6) THEN
          FAILED ("INCORRECT SLICE (VALUE)");
     END IF;

     Y.ALL := (1, 2, 3);
     X := IDENT (Y);
     BEGIN
          CREATE (1, 4, 4, X) (2 .. 4) := (10, 11, 12);
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION FOR SLICE (ASSIGNMENT)");
     END;

     IF X = NULL OR X = NEW SUBDESIGNATED OR NOT (X = Y) OR
        X = CREATE (2, 3, 4, X) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= Y OR NOT (X /= NULL) OR NOT (X /= CREATE (2, 3, 4, X)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) OR CREATE (2, 3, 4, X) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (CREATE (2, 3, 4, X) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     RESULT;
END C34007V;
