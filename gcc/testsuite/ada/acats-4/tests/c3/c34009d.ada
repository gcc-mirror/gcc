-- C34009D.ADA

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
--     (IMPLICITLY) FOR DERIVED NON-LIMITED PRIVATE TYPES WITH
--     DISCRIMINANTS.

-- HISTORY:
--     JRK 08/31/87  CREATED ORIGINAL TEST.
--     WMC 03/13/92  REVISED TYPE'SIZE CHECKS.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34009D IS

     PACKAGE PKG IS

          MAX_LEN : CONSTANT := 10;

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. MAX_LEN;

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 1) IS PRIVATE;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            J : INTEGER;
                            F : FLOAT;
                            X : PARENT  -- TO RESOLVE OVERLOADING.
                          ) RETURN PARENT;

          FUNCTION CON ( B : BOOLEAN;
                         L : LENGTH;
                         I : INTEGER;
                         S : STRING;
                         J : INTEGER
                       ) RETURN PARENT;

          FUNCTION CON ( B : BOOLEAN;
                         L : LENGTH;
                         I : INTEGER;
                         F : FLOAT
                       ) RETURN PARENT;

     PRIVATE

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
               RECORD
                    I : INTEGER;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L);
                              J : INTEGER;
                         WHEN FALSE =>
                              F : FLOAT := 5.0;
                    END CASE;
               END RECORD;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT (IDENT_BOOL (TRUE), IDENT_INT (3));

     X : T;
     W : PARENT;
     B : BOOLEAN := FALSE;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               J : INTEGER;
               F : FLOAT;
               X : PARENT
             ) RETURN PARENT
          IS
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN (TRUE, L, I, S, J);
                    WHEN FALSE =>
                         RETURN (FALSE, L, I, F);
               END CASE;
          END CREATE;

          FUNCTION CON
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               J : INTEGER
             ) RETURN PARENT
          IS
          BEGIN
               RETURN (TRUE, L, I, S, J);
          END CON;

          FUNCTION CON
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               F : FLOAT
             ) RETURN PARENT
          IS
          BEGIN
               RETURN (FALSE, L, I, F);
          END CON;

     END PKG;

BEGIN
     TEST ("C34009D", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "NON-LIMITED PRIVATE TYPES WITH DISCRIMINANTS");

     X := CON (TRUE, 3, 2, "AAA", 2);
     W := CON (TRUE, 3, 2, "AAA", 2);

     IF EQUAL (3, 3) THEN
          X := CON (TRUE, 3, 1, "ABC", 4);
     END IF;
     IF X /= CON (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= CON (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= CON (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := CON (TRUE, 3, 1, "ABC", 4);
     END IF;
     IF T (W) /= CON (TRUE, 3, 1, "ABC", 4) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= CON (TRUE, 3, 1, "ABC", 4) OR
        PARENT (CREATE (FALSE, 2, 3, "XX", 5, 6.0, X)) /=
        CON (FALSE, 2, 3, 6.0) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF X.B /= TRUE OR X.L /= 3 OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . B /= FALSE OR
        CREATE (FALSE, 2, 3, "XX", 5, 6.0, X) . L /= 2 THEN
          FAILED ("INCORRECT SELECTION (DISCRIMINANT)");
     END IF;

     IF X = CON (TRUE, 3, 1, "ABC", 5) OR
        X = CON (FALSE, 2, 3, 6.0) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= CON (TRUE, 3, 1, "ABC", 4) OR
        NOT (X /= CON (FALSE, 2, 3, 6.0)) THEN
          FAILED ("INCORRECT /=");
     END IF;

     IF NOT (X IN T) OR CON (FALSE, 2, 3, 6.0) IN T THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T OR NOT (CON (FALSE, 2, 3, 6.0) NOT IN T) THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;

     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF NOT X'CONSTRAINED THEN
          FAILED ("INCORRECT OBJECT'CONSTRAINED");
     END IF;

     IF T'SIZE <= 0 THEN
          FAILED ("INCORRECT TYPE'SIZE");
     END IF;

     IF X'SIZE   < T'SIZE OR
        X.B'SIZE < BOOLEAN'SIZE OR
        X.L'SIZE < LENGTH'SIZE THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     RESULT;
END C34009D;
