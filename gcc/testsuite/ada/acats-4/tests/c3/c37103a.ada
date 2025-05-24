-- C37103A.ADA

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
-- CHECK THAT DISCRIMINANTS MAY BE BOOLEAN, CHARACTER, USER_ENUM,
-- INTEGER, DERIVED CHARACTER, DERIVED USER_ENUM, DERIVED INTEGER,
-- AND DERIVED DERIVED USER_ENUM.

-- DAT 5/18/81
-- SPS 10/25/82

WITH REPORT; USE REPORT;

PROCEDURE C37103A IS
BEGIN
     TEST ("C37103A", "MANY DIFFERENT DISCRIMINANT TYPES");
     DECLARE
          PACKAGE P1 IS
               TYPE ENUM IS (A, Z, Q, 'W', 'A');
          END P1;

          PACKAGE P2 IS
               TYPE E2 IS NEW P1.ENUM;
          END P2;

          PACKAGE P3 IS
               TYPE E3 IS NEW P2.E2;
          END P3;

          USE P1, P2, P3;
          TYPE INT IS NEW INTEGER RANGE -3 .. 7;
          TYPE CHAR IS NEW CHARACTER;
          TYPE R1 (D : ENUM) IS RECORD NULL; END RECORD;
          TYPE R2 (D : INTEGER) IS RECORD NULL; END RECORD;
          TYPE R3 (D : BOOLEAN) IS RECORD NULL; END RECORD;
          TYPE R4 (D : CHARACTER) IS RECORD NULL; END RECORD;
          TYPE R5 (D : CHAR) IS RECORD NULL; END RECORD;
          TYPE R6 (D : E2) IS RECORD NULL; END RECORD;
          TYPE R7 (D : E3) IS RECORD NULL; END RECORD;
          TYPE R8 (D : INT) IS RECORD NULL; END RECORD;
          O1 : R1(A) := (D => A);
          O2 : R2(3) := (D => 3);
          O3 : R3(TRUE) := (D => TRUE);
          O4 : R4(ASCII.NUL) := (D => ASCII.NUL);
          O5 : R5('A') := (D => 'A');
          O6 : R6('A') := (D => 'A');
          O7 : R7(A) := (D => A);
          O8 : R8(2) := (D => 2);
     BEGIN
          IF O1.D /= A
          OR O2.D /= 3
          OR NOT O3.D
          OR O4.D IN 'A' .. 'Z'
          OR O5.D /= 'A'
          OR O6.D /= 'A'
          OR O7.D /= A
          OR O8.D /= 2
          THEN FAILED ("WRONG DISCRIMINANT VALUE");
          END IF;
     END;

     RESULT;
END C37103A;
