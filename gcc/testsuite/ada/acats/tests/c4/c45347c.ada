-- C45347C.ADA

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
-- CHECK THAT CATENATION IS DEFINED FOR PRIVATE TYPES AS COMPONENT
-- TYPES.

-- JWC 11/15/85

WITH REPORT; USE REPORT;

PROCEDURE C45347C IS

BEGIN

     TEST ("C45347C", "CHECK THAT CATENATION IS DEFINED " &
                      "FOR PRIVATE TYPES AS COMPONENT TYPES");

     DECLARE

          PACKAGE PKG IS
               TYPE PRIV IS PRIVATE;
               ONE : CONSTANT PRIV;
               TWO : CONSTANT PRIV;
               THREE : CONSTANT PRIV;
               FOUR : CONSTANT PRIV;
          PRIVATE
               TYPE PRIV IS NEW INTEGER;
               ONE : CONSTANT PRIV := 1;
               TWO : CONSTANT PRIV := 2;
               THREE : CONSTANT PRIV := 3;
               FOUR : CONSTANT PRIV := 4;
          END PKG;

          USE PKG;

          SUBTYPE INT IS INTEGER RANGE 1 .. 4;
          TYPE A IS ARRAY ( INT RANGE <>) OF PRIV;

          P1 : PRIV := FOUR;
          P2 : PRIV := ONE;

          A1 : A(1 .. 2) := (ONE, TWO);
          A2 : A(1 .. 2) := (THREE, FOUR);
          A3 : A(1 .. 4) := (ONE, TWO, THREE, FOUR);
          A4 : A(1 .. 4);
          A5 : A(1 .. 4) := (FOUR, THREE, TWO, ONE);

     BEGIN

          A4 := A1 & A2;

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR TWO ARRAYS OF " &
                       "PRIVATE");
          END IF;

          A4 := A5;

          A4 := A1 & A2(1) & P1;

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR ARRAY OF PRIVATE, " &
                       "AND PRIVATE");
          END IF;

          A4 := A5;

          A4 := P2 & (A1(2) & A2);

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR PRIVATE, AND ARRAY " &
                       "OF PRIVATE");
          END IF;

          A4 := A5;

          A4 := P2 & A1(2) & (A2(1) & P1);

          IF A3 /= A4 THEN
               FAILED ("INCORRECT CATENATION FOR PRIVATE");
          END IF;

     END;

     RESULT;

END C45347C;
