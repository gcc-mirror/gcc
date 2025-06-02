-- C34008A.ADA

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
--     (IMPLICITLY) FOR DERIVED TASK TYPES.

-- HISTORY:
--     JRK 08/27/87  CREATED ORIGINAL TEST.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     DTN 11/30/95  REMOVED ATTIBUTES OF NON-OBJECTS.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34008A IS

     PACKAGE PKG IS

          TASK TYPE PARENT IS
               ENTRY E (I : IN OUT INTEGER);
               ENTRY F (1 .. 3) (I : INTEGER; J : OUT INTEGER);
               ENTRY G;
               ENTRY H (1 .. 3);
               ENTRY R (I : OUT INTEGER);
               ENTRY W (I : INTEGER);
          END PARENT;

          FUNCTION ID (X : PARENT) RETURN INTEGER;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT;

     TASK TYPE AUX;

     X : T;
     W : PARENT;
     B : BOOLEAN := FALSE;
     I : INTEGER := 0;
     J : INTEGER := 0;
     A1, A2 : AUX;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION V RETURN T IS
     BEGIN
          RETURN X;
     END V;

     PACKAGE BODY PKG IS

          TASK BODY PARENT IS
               N : INTEGER := 1;
          BEGIN
               LOOP
                    SELECT
                         ACCEPT E (I : IN OUT INTEGER) DO
                              I := I + N;
                         END E;
                    OR
                         ACCEPT F (2) (I : INTEGER; J : OUT INTEGER) DO
                              J := I + N;
                         END F;
                    OR
                         ACCEPT G DO
                              WHILE H(2)'COUNT < 2 LOOP
                                   DELAY 5.0;
                              END LOOP;
                              ACCEPT H (2) DO
                                   IF E'COUNT    /= 0 OR
                                      F(1)'COUNT /= 0 OR
                                      F(2)'COUNT /= 0 OR
                                      F(3)'COUNT /= 0 OR
                                      G'COUNT    /= 0 OR
                                      H(1)'COUNT /= 0 OR
                                      H(2)'COUNT /= 1 OR
                                      H(3)'COUNT /= 0 OR
                                      R'COUNT    /= 0 OR
                                      W'COUNT    /= 0 THEN
                                        FAILED ("INCORRECT 'COUNT");
                                   END IF;
                              END H;
                              ACCEPT H (2);
                         END G;
                    OR
                         ACCEPT R (I : OUT INTEGER) DO
                              I := N;
                         END R;
                    OR
                         ACCEPT W (I : INTEGER) DO
                              N := I;
                         END W;
                    OR
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END PARENT;

          FUNCTION ID (X : PARENT) RETURN INTEGER IS
               I : INTEGER;
          BEGIN
               X.R (I);
               RETURN I;
          END ID;

     END PKG;

     TASK BODY AUX IS
     BEGIN
          X.H (2);
     END AUX;

BEGIN
     TEST ("C34008A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED TASK " &
                      "TYPES");

     X.W (IDENT_INT (2));
     IF ID (X) /= 2 THEN
          FAILED ("INCORRECT INITIALIZATION");
     END IF;

     IF ID (T'(X)) /= 2 THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF ID (T (X)) /= 2 THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     W.W (IDENT_INT (3));
     IF ID (T (W)) /= 3 THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF ID (PARENT (X)) /= 2 THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     I := 5;
     X.E (I);
     IF I /= 7 THEN
          FAILED ("INCORRECT SELECTION (ENTRY)");
     END IF;

     I := 5;
     X.F (IDENT_INT (2)) (I, J);
     IF J /= 7 THEN
          FAILED ("INCORRECT SELECTION (FAMILY)");
     END IF;

     IF NOT (X IN T) THEN
          FAILED ("INCORRECT ""IN""");
     END IF;

     IF X NOT IN T THEN
          FAILED ("INCORRECT ""NOT IN""");
     END IF;


     B := FALSE;
     A (X'ADDRESS);
     IF NOT B THEN
          FAILED ("INCORRECT OBJECT'ADDRESS");
     END IF;

     IF NOT X'CALLABLE THEN
          FAILED ("INCORRECT OBJECT'CALLABLE");
     END IF;

     IF NOT V'CALLABLE THEN
          FAILED ("INCORRECT VALUE'CALLABLE");
     END IF;

     X.G;

     IF X'SIZE < T'SIZE THEN
          FAILED ("INCORRECT OBJECT'SIZE");
     END IF;

     IF T'STORAGE_SIZE < 0 THEN
          FAILED ("INCORRECT TYPE'STORAGE_SIZE");
     END IF;

     IF X'STORAGE_SIZE < 0 THEN
          FAILED ("INCORRECT OBJECT'STORAGE_SIZE");
     END IF;

     IF X'TERMINATED THEN
          FAILED ("INCORRECT OBJECT'TERMINATED");
     END IF;

     IF V'TERMINATED THEN
          FAILED ("INCORRECT VALUE'TERMINATED");
     END IF;

     RESULT;
END C34008A;
