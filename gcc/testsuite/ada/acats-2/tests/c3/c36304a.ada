-- C36304A.ADA

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
-- CHECK THAT BOUNDS OF CONSTANT STRING OBJECTS IF NOT GIVEN IN
-- THE DECLARATIONS ARE DETERMINED BY THE STRINGS' INITIAL VALUES.

-- DAT 2/17/81
-- JBG 8/21/83

WITH REPORT;
PROCEDURE C36304A IS

     USE REPORT;

     I3 : INTEGER := IDENT_INT (3);

     S3 : CONSTANT STRING := "ABC";
     S0 : CONSTANT STRING := "";
     S1 : CONSTANT STRING := "A";
     S2 : CONSTANT STRING := "AB";
     S5 : CONSTANT STRING := "ABCDE";
     S3A : CONSTANT STRING (I3 .. I3 + 2) := S3(I3 - 2 .. I3);
     S3C : CONSTANT STRING := S3A;
     S3D : CONSTANT STRING := S3C & "";
     S3E : CONSTANT STRING := S3D;
     X3 : CONSTANT STRING := (I3 .. 5 => 'X');
     Y3 : CONSTANT STRING := X3;
     Z0 : CONSTANT STRING := (-3..-5 => 'A');

     PROCEDURE C (S : STRING; 
                  FIRST, LAST, LENGTH : INTEGER; 
                  ID : STRING) IS
     BEGIN
          IF S'FIRST /= FIRST THEN
               FAILED ("'FIRST IS " & INTEGER'IMAGE(S'FIRST) &
                       " INSTEAD OF " & INTEGER'IMAGE(FIRST) &
                       " FOR " & ID);
          END IF;

          IF S'LAST /= LAST THEN
               FAILED ("'LAST IS " & INTEGER'IMAGE(S'LAST) &
                       " INSTEAD OF " & INTEGER'IMAGE(LAST) &
                       " FOR " & ID);
          END IF;

          IF S'LENGTH /= LENGTH THEN
               FAILED ("'LENGTH IS " & INTEGER'IMAGE(S'LENGTH) &
                       " INSTEAD OF " & INTEGER'IMAGE(LENGTH) &
                       " FOR " & ID);
          END IF;
     END C;

BEGIN
     TEST ("C36304A", "CHECK UNUSUAL CONSTANT STRING BOUNDS");


     C(S0, 1, 0, 0, "S0");
     C(S1, 1, 1, 1, "S1");
     C(S2, 1, 2, 2, "S2");
     C(S5, 1, 5, 5, "S5");
     C(S3, 1, 3, 3, "S3");
     C(S3C, 3, 5, 3, "S3C");
     C(S3D, 3, 5, 3, "S3D");
     C(S3E, 3, 5, 3, "S3E");
     C(X3, 3, 5, 3, "X3");
     C(Y3, 3, 5, 3, "Y3");
     C(Z0, IDENT_INT(-3), IDENT_INT(-5), IDENT_INT(0), "Z0");

     RESULT;
END C36304A;
