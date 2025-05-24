-- CC3011A.ADA

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
-- CHECK THAT SUBPROGRAMS THAT WOULD HAVE THE SAME SPECIFICATION
-- AFTER GENERIC INSTANTIATION MAY BE DECLARED IN THE SAME
-- DECLARATIVE PART, AND THAT CALLS WITHIN THE INSTANTIATED UNIT ARE
-- UNAMBIGUOUS.  CHECK THAT CALLS FROM OUTSIDE THE UNIT ARE UNAMBIGUOUS
-- IF FORMAL PARAMETER NAMES ARE USED OR IF ONLY ONE OF THE EQUIVALENT
-- PROGRAMS APPEARS IN THE VISIBLE PART OF THE PACKAGE.

-- DAT 9/18/81
-- SPS 10/19/82

WITH REPORT; USE REPORT;

PROCEDURE CC3011A IS
BEGIN
     TEST ("CC3011A", "CHECK SUBPROGRAMS IN GENERIC PACKAGES WITH SAME"
          & " SPECIFICATION AFTER GENERIC PARAMETER SUBSTITUTION");

     DECLARE
          C : INTEGER := 0;

          GENERIC
               TYPE S IS ( <> );
               TYPE T IS PRIVATE;
               TYPE U IS RANGE <> ;
               VT : T;
          PACKAGE PKG IS
               PROCEDURE P1 (X : S);
          PRIVATE
               PROCEDURE P1 (X : T);
               VS : S := S'FIRST;
               VU : U := U'FIRST;
          END PKG;

          GENERIC
               TYPE S IS (<>);
               TYPE T IS RANGE <>;
          PACKAGE PP IS
               PROCEDURE P1 (D: S);
               PROCEDURE P1 (X: T);
          END PP;

          PACKAGE BODY PKG IS
               PROCEDURE P1 (X : S) IS 
               BEGIN
                    C := C + 1;
               END P1;
               PROCEDURE P1 (X : T) IS
               BEGIN
                    C := C + 2;
               END P1;
               PROCEDURE P1 (X : U) IS
               BEGIN
                    C := C + 4;
               END P1;
          BEGIN
               C := 0;
               P1 (VS);
               IF C /= IDENT_INT (1) THEN
                    FAILED ("WRONG P1 CALLED -S");
               END IF;
               C := 0;
               P1 (VT);
               IF C /= IDENT_INT (2) THEN
                    FAILED ("WRONG P1 CALLED -T");
               END IF;
               C := 0;
               P1 (VU);
               IF C /= IDENT_INT (4) THEN
                    FAILED ("WRONG P1 CALLED -U");
               END IF;
               C := 0;
          END PKG;

          PACKAGE BODY PP IS
               PROCEDURE P1 (D: S) IS
               BEGIN
                    C := C + 3;
               END P1;
               PROCEDURE P1 (X: T) IS
               BEGIN
                    C := C + 5;
               END P1;
          BEGIN
               NULL;
          END PP;

          PACKAGE NP IS NEW PKG (INTEGER, INTEGER, INTEGER, 7);
          PACKAGE NPP IS NEW PP (INTEGER, INTEGER);
     BEGIN
          NP.P1 (4);
          IF C /= IDENT_INT (1) THEN
               FAILED ("INCORRECT OVERLOADING ON FORMAL TYPES");
          END IF;
          C := 0;
          NPP.P1 (D => 3);
          IF C /= IDENT_INT (3) THEN
               FAILED ("INCORRECT CALL TO P1 WITH D PARAMETER");
          END IF;
          C := 0;
          NPP.P1 (X => 7);
          IF C /= IDENT_INT (5) THEN
               FAILED ("INCORRECT CALL TO P1 WITH X PARAMETER");
          END IF;
     END;

     RESULT;
END CC3011A;
