-- C34006A.ADA

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
--     (IMPLICITLY) FOR DERIVED RECORD TYPES WITHOUT DISCRIMINANTS
--     AND WITH NON-LIMITED COMPONENT TYPES.

-- HISTORY:
--     JRK 09/22/86  CREATED ORIGINAL TEST.
--     BCB 09/26/88  REMOVED COMPARISONS INVOLVING SIZE.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34006A IS

     SUBTYPE COMPONENT IS INTEGER;

     TYPE PARENT IS
          RECORD
               C : COMPONENT;
               B : BOOLEAN := TRUE;
          END RECORD;

     TYPE T IS NEW PARENT;

     X : T         := (2, FALSE);
     K : INTEGER   := X'SIZE;
     W : PARENT    := (2, FALSE);
     C : COMPONENT := 1;
     B : BOOLEAN   := FALSE;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     FUNCTION IDENT (X : T) RETURN T IS
     BEGIN
          IF EQUAL (X.C, X.C) THEN
               RETURN X;                          -- ALWAYS EXECUTED.
          END IF;
          RETURN (-1, FALSE);
     END IDENT;

BEGIN
     TEST ("C34006A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "RECORD TYPES WITHOUT DISCRIMINANTS AND WITH " &
                      "NON-LIMITED COMPONENT TYPES");

     X := IDENT ((1, TRUE));
     IF X /= (1, TRUE) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= (1, TRUE) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= (1, TRUE) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     IF EQUAL (3, 3) THEN
          W := (1, TRUE);
     END IF;
     IF T (W) /= (1, TRUE) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= (1, TRUE) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF IDENT ((1, TRUE)) /= (1, TRUE) THEN
          FAILED ("INCORRECT AGGREGATE");
     END IF;

     IF X.C /= 1 OR X.B /= TRUE THEN
          FAILED ("INCORRECT SELECTION (VALUE)");
     END IF;

     X.C := IDENT_INT (3);
     X.B := IDENT_BOOL (FALSE);
     IF X /= (3, FALSE) THEN
          FAILED ("INCORRECT SELECTION (ASSIGNMENT)");
     END IF;

     X := IDENT ((1, TRUE));
     IF X = IDENT ((1, FALSE)) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= IDENT ((1, TRUE)) THEN
          FAILED ("INCORRECT /=");
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
          FAILED ("INCORRECT 'ADDRESS");
     END IF;

     IF X.C'FIRST_BIT < 0 THEN
          FAILED ("INCORRECT 'FIRST_BIT");
     END IF;

     IF X.C'LAST_BIT < 0 OR
        X.C'LAST_BIT - X.C'FIRST_BIT + 1 /= X.C'SIZE THEN
          FAILED ("INCORRECT 'LAST_BIT");
     END IF;

     IF X.C'POSITION < 0 THEN
          FAILED ("INCORRECT 'POSITION");
     END IF;


     RESULT;
END C34006A;
