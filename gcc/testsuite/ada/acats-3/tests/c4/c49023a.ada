-- C49023A.ADA

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
-- CHECK THAT A CONSTANT DECLARED BY AN OBJECT DECLARATION CAN BE USED
-- IN A STATIC EXPRESSION IF THE CONSTANT WAS DECLARED WITH A STATIC
-- SUBTYPE AND INITIALIZED WITH A STATIC EXPRESSION.

-- L.BROWN  10/01/86

WITH REPORT; USE REPORT;
PROCEDURE  C49023A  IS

BEGIN
     TEST("C49023A","A CONSTANT DECLARED BY AN OBJECT DECLARATION "&
                    "UNDER CERTAIN CONDITIONS CAN BE USED IN A "&
                    "STATIC EXPRESSION");
     DECLARE
          TYPE ENUM IS (RED,GREEN,BLUE,YELLOW);
          SUBTYPE SENUM IS ENUM RANGE RED .. BLUE;
          CONEN : CONSTANT SENUM := GREEN;
          TYPE INT IS RANGE 1 .. 10;
          SUBTYPE SINT IS INT RANGE 1 .. 5;
          CONIN : CONSTANT SINT := 3;
          TYPE FLT IS DIGITS 3 RANGE 0.0 .. 25.0;
          SUBTYPE SFLT IS FLT RANGE 10.0 .. 20.0;
          CONFL : CONSTANT SFLT := 11.0;
          TYPE FIX IS DELTA 0.25 RANGE 0.0 .. 25.0;
          SUBTYPE SFIX IS FIX RANGE 0.0 .. 12.0;
          CONFI : CONSTANT SFIX := 0.25;
          CAS_EN : ENUM := CONEN;
          TYPE ITEG IS RANGE 1 .. CONIN;
          TYPE FLTY IS DIGITS CONIN;
          TYPE FIXY IS DELTA CONFI RANGE 0.0 .. 10.0;
          TYPE REAL IS DELTA 0.25 RANGE 0.0 .. 11.0;
          TYPE FIXTY IS DELTA 0.25 RANGE 0.0 .. CONFL;

          FUNCTION IDENT_REAL (X : REAL) RETURN REAL;

          PACKAGE P IS
               TYPE T IS PRIVATE;
               CON1 : CONSTANT T;
          PRIVATE
               TYPE T IS NEW INTEGER;
               CON1 : CONSTANT T := 10;
               TYPE NINT IS RANGE 1 .. CON1;
          END P;
          PACKAGE BODY P IS
               TYPE CON2 IS RANGE CON1 .. 50;
          BEGIN
               IF NINT'LAST /= NINT(IDENT_INT(10)) THEN
                    FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 1");
               END IF;
               IF CON2'FIRST /= CON2(IDENT_INT(10)) THEN
                    FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 2");
               END IF;
          END P;

          FUNCTION IDENT_REAL (X : REAL) RETURN REAL IS
          BEGIN
               IF EQUAL(3,3) THEN
                    RETURN X;
               ELSE
                    RETURN 0.0;
               END IF;
          END IDENT_REAL;

     BEGIN

          IF ITEG'LAST /= ITEG(IDENT_INT(3))  THEN
               FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 3");
          END IF;

          IF FLTY'DIGITS /= IDENT_INT(3)  THEN
               FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 4");
          END IF;

          IF FIXY'DELTA /= IDENT_REAL(0.25)  THEN
               FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 5");
          END IF;

          IF FIXTY'LAST /= FIXTY(IDENT_REAL(11.0)) THEN
               FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 6");
          END IF;

          CASE CAS_EN IS
               WHEN CONEN =>
                    CAS_EN := RED;
               WHEN OTHERS =>
                    FAILED("INCORRECT VALUE FOR STATIC EXPRESSION 7");
          END CASE;

     END;

     RESULT;

END C49023A;
