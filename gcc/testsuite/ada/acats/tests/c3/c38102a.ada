-- C38102A.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE DECLARATION CAN BE GIVEN FOR ANY TYPE.
-- FULL DECLARATIONS FOR INTEGER, ENUMERATION, CONSTRAINED AND
-- UNCONSTRAINED ARRAYS, RECORDS WITHOUT DISCRIMINANTS,
-- AN ACCESS TYPE, OR TYPES DERIVED FROM ANY OF THE ABOVE.

-- (FLOAT, FIXED, TASKS AND RECORDS WITH DISCRIMINANTS ARE CHECKED
-- IN OTHER TESTS).

-- DAT 3/24/81
-- SPS 10/25/82
-- SPS 2/17/82

WITH REPORT; USE REPORT;

PROCEDURE C38102A IS
BEGIN
     TEST ("C38102A", "ANY TYPE MAY BE INCOMPLETE");

     DECLARE

          TYPE X1;
          TYPE X2;
          TYPE X3;
          TYPE X4;
          TYPE X5;
          TYPE X6;
          TYPE X7;
          TYPE X8;
     
          TYPE D1;
          TYPE D2;
          TYPE D3;
          TYPE D4;
          TYPE D5;
          TYPE D6;
     
          TYPE X1 IS RANGE 1 .. 10;
          TYPE X2 IS (TRUE, FALSE, MAYBE, GREEN);
          TYPE X3 IS ARRAY (1 .. 3) OF STRING (1..10);
          TYPE X4 IS ARRAY (NATURAL RANGE <> ) OF X3;
          TYPE AR1 IS ARRAY (X2) OF X3;
          TYPE X5 IS RECORD
               C1 : X4 (1..3);
               C2 : AR1;
          END RECORD;
          TYPE X6 IS ACCESS X8;
          TYPE X7 IS ACCESS X6;
          TYPE X8 IS ACCESS X6;
     
          TYPE D1 IS NEW X1;
          TYPE D2 IS NEW X2;
          TYPE D3 IS NEW X3;
          TYPE D4 IS NEW X4;
          TYPE D5 IS NEW X5;
          SUBTYPE D7 IS X7;
          SUBTYPE D8 IS X8;
          TYPE D6 IS ACCESS D8;
     
          PACKAGE P IS
     
               TYPE X1;
               TYPE X2;
               TYPE X3;
               TYPE X4;
               TYPE X5;
               TYPE X6;
               TYPE X7 IS PRIVATE;
               TYPE X8 IS LIMITED PRIVATE;
     
               TYPE D1;
               TYPE D2;
               TYPE D3;
               TYPE D4;
               TYPE D5;
               TYPE D6;
     
               TYPE X1 IS RANGE 1 .. 10;
               TYPE X2 IS (TRUE, FALSE, MAYBE, GREEN);
               TYPE X3 IS ARRAY (1 .. 3) OF STRING (1..10);
               TYPE X4 IS ARRAY (NATURAL RANGE <> ) OF X3;
               TYPE AR1 IS ARRAY (X2) OF X3;
               TYPE X5 IS RECORD
                    C1 : X4 (1..3);
                    C2 : AR1;
               END RECORD;
               TYPE X6 IS ACCESS X8;
     
               TYPE D1 IS RANGE 1 .. 10;
               TYPE D2 IS NEW X2;
               TYPE D3 IS NEW X3;
               TYPE D4 IS NEW X4;
               TYPE D5 IS NEW X5;
               TYPE D6 IS NEW X6;
               SUBTYPE D7 IS X7;
               SUBTYPE D8 IS X8;
               TYPE D9 IS ACCESS D8;

               VX7 : CONSTANT X7;
     
          PRIVATE
     
               TYPE X7 IS RECORD
                    C1 : X1;
                    C3 : X3;
                    C5 : X5;
                    C6 : X6;
                    C8 : D9;
               END RECORD;

               V3 : X3 := (X3'RANGE => "ABCDEFGHIJ");
               TYPE A7 IS ACCESS X7;
               TYPE X8 IS ARRAY (V3'RANGE) OF A7;
     
               VX7 : CONSTANT X7 := (3, V3, ((1..3=>V3),
                    (TRUE..GREEN=>V3)), NULL,
                    NEW D8);
          END P;
          USE P;

          VD7: P.D7;

          PACKAGE BODY P IS
          BEGIN
               VD7 := D7(VX7);
          END P;

     BEGIN
          IF VX7 /= P.X7(VD7) THEN
               FAILED ("WRONG VALUE SOMEWHERE");
          END IF;
     END;

     RESULT;
END C38102A;
