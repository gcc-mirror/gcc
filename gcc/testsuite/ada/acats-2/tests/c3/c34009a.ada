-- C34009A.ADA

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
--     (IMPLICITLY) FOR DERIVED NON-LIMITED PRIVATE TYPES WITHOUT
--     DISCRIMINANTS.

-- HISTORY:
--     JRK 08/28/87  CREATED ORIGINAL TEST.
--     BCB 09/26/88  REMOVED COMPARISON INVOLVING OBJECT SIZE.
--     PWN 11/30/94  REMOVED 'BASE USE ILLEGAL IN ADA 9X.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;

PROCEDURE C34009A IS

     PACKAGE PKG IS

          TYPE PARENT IS PRIVATE;

          FUNCTION CREATE (X : INTEGER) RETURN PARENT;

          FUNCTION CON (X : INTEGER) RETURN PARENT;

     PRIVATE

          TYPE PARENT IS NEW INTEGER;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT;

     X : T;
     K : INTEGER := X'SIZE;
     W : PARENT;
     B : BOOLEAN := FALSE;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          B := IDENT_BOOL (TRUE);
     END A;

     PACKAGE BODY PKG IS

          FUNCTION CREATE (X : INTEGER) RETURN PARENT IS
          BEGIN
               RETURN PARENT (IDENT_INT (X));
          END CREATE;

          FUNCTION CON (X : INTEGER) RETURN PARENT IS
          BEGIN
               RETURN PARENT (X);
          END CON;

     END PKG;

BEGIN
     TEST ("C34009A", "CHECK THAT THE REQUIRED PREDEFINED OPERATIONS " &
                      "ARE DECLARED (IMPLICITLY) FOR DERIVED " &
                      "NON-LIMITED PRIVATE TYPES WITHOUT " &
                      "DISCRIMINANTS");

     X := CREATE (30);
     IF X /= CON (30) THEN
          FAILED ("INCORRECT :=");
     END IF;

     IF T'(X) /= CON (30) THEN
          FAILED ("INCORRECT QUALIFICATION");
     END IF;

     IF T (X) /= CON (30) THEN
          FAILED ("INCORRECT SELF CONVERSION");
     END IF;

     W := CREATE (-30);
     IF T (W) /= CON (-30) THEN
          FAILED ("INCORRECT CONVERSION FROM PARENT");
     END IF;

     IF PARENT (X) /= CON (30) THEN
          FAILED ("INCORRECT CONVERSION TO PARENT");
     END IF;

     IF X = CON (0) THEN
          FAILED ("INCORRECT =");
     END IF;

     IF X /= CON (30) THEN
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

     RESULT;
END C34009A;
