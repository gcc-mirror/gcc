-- CD2D11A.ADA

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
--     CHECK THAT IF A SMALL SPECIFICATION IS GIVEN FOR A
--     FIXED POINT TYPE, THEN ARITHMETIC OPERATIONS ON VALUES OF THE
--     TYPE ARE NOT AFFECTED BY THE REPRESENTATION CLAUSE.

-- HISTORY:
--     BCB 09/01/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT; USE REPORT;
PROCEDURE CD2D11A IS

     BASIC_SMALL : CONSTANT := 2.0 ** (-4);

     TYPE BASIC_TYPE IS DELTA 2.0 ** (-4) RANGE -4.0 .. 4.0;

     TYPE CHECK_TYPE IS DELTA 1.0 RANGE -4.0 .. 4.0;

     FOR CHECK_TYPE'SMALL USE BASIC_SMALL;

     CNEG1 : CHECK_TYPE := -3.5;
     CNEG2 : CHECK_TYPE := CHECK_TYPE (-1.0/3.0);
     CPOS1 : CHECK_TYPE := CHECK_TYPE (4.0/6.0);
     CPOS2 : CHECK_TYPE :=  3.5;
     CZERO : CHECK_TYPE;

     TYPE ARRAY_TYPE IS ARRAY (0 .. 3) OF CHECK_TYPE;
     CHARRAY : ARRAY_TYPE :=
          (-3.5, CHECK_TYPE (-1.0/3.0), CHECK_TYPE (4.0/6.0), 3.5);

     TYPE REC_TYPE IS RECORD
          COMPN1 : CHECK_TYPE := -3.5;
          COMPN2 : CHECK_TYPE := CHECK_TYPE (-1.0/3.0);
          COMPP1 : CHECK_TYPE := CHECK_TYPE (4.0/6.0);
          COMPP2 : CHECK_TYPE :=  3.5;
     END RECORD;

     CHREC : REC_TYPE;

     FUNCTION IDENT (FX : CHECK_TYPE) RETURN CHECK_TYPE IS
     BEGIN
          IF EQUAL (3, 3) THEN
               RETURN FX;
          ELSE
               RETURN 0.0;
          END IF;
     END IDENT;

     PROCEDURE PROC (N1_IN, P1_IN      :        CHECK_TYPE;
                     N2_INOUT,P2_INOUT : IN OUT CHECK_TYPE;
                     CZOUT             :    OUT CHECK_TYPE) IS
     BEGIN

          IF IDENT (N1_IN) + P1_IN NOT IN
                  -2.875 .. -2.8125 OR
             P2_INOUT  - IDENT (P1_IN) NOT IN
                  2.8125 .. 2.875 THEN
               FAILED ("INCORRECT RESULTS FOR " &
                       "BINARY ADDING OPERATORS - 1");
          END IF;

          IF +IDENT (N2_INOUT) NOT IN -0.375 .. -0.3125 OR
             IDENT (-P1_IN) NOT IN -0.6875 .. -0.625 THEN
               FAILED ("INCORRECT RESULTS FOR " &
                       "UNARY ADDING OPERATORS - 1");
          END IF;

          IF CHECK_TYPE (N1_IN * IDENT (P1_IN)) NOT IN
               -2.4375 .. -2.1875 OR
             CHECK_TYPE (IDENT (N2_INOUT) / P2_INOUT) NOT IN
               -0.125 .. -0.0625 THEN
               FAILED ("INCORRECT RESULTS FOR " &
                       "MULTIPLYING OPERATORS - 1");
          END IF;

          IF ABS IDENT (N2_INOUT) NOT IN 0.3125 .. 0.375 OR
             IDENT (ABS P1_IN) NOT IN 0.625 .. 0.6875 THEN
               FAILED ("INCORRECT RESULTS FOR " &
                       "ABSOLUTE VALUE OPERATORS - 1");
          END IF;

          CZOUT := 0.0;

     END PROC;

BEGIN
     TEST ("CD2D11A", "CHECK THAT IF A SMALL SPECIFICATION IS " &
                      "GIVEN FOR AN FIXED POINT TYPE, THEN " &
                      "ARITHMETIC OPERATIONS ON VALUES OF THE " &
                      "TYPE ARE NOT AFFECTED BY THE REPRESENTATION " &
                      "CLAUSE");

     PROC (CNEG1, CPOS1, CNEG2, CPOS2, CZERO);

     IF IDENT (CZERO) /= 0.0 THEN
          FAILED ("INCORRECT VALUE FOR OUT PARAMETER");
     END IF;

     IF IDENT (CNEG1) + CPOS1 NOT IN -2.875 .. -2.8125 OR
        CPOS2  - IDENT (CPOS1) NOT IN 2.8125 .. 2.875 THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 2");
     END IF;

     IF +IDENT (CNEG2) NOT IN -0.375 .. -0.3125 OR
        IDENT (-CPOS1) NOT IN -0.6875 .. -0.625 THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 2");
     END IF;

     IF CHECK_TYPE (CNEG1 * IDENT (CPOS1)) NOT IN -2.4375 .. -2.1875 OR
        CHECK_TYPE (IDENT (CNEG2) / CPOS2) NOT IN
               -0.125 .. -0.0625 THEN
          FAILED ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 2");
     END IF;

     IF ABS IDENT (CNEG2) NOT IN 0.3125 .. 0.375 OR
        IDENT (ABS CPOS1) NOT IN 0.625 .. 0.6875 THEN
          FAILED ("INCORRECT RESULTS FOR ABSOLUTE VALUE " &
                  "OPERATORS - 2");
     END IF;

     IF IDENT (CPOS1) NOT IN 0.625 .. 0.6875 OR
            CNEG2 IN -0.25 .. 0.0 OR
            IDENT (CNEG2) IN -1.0 .. -0.4375 THEN
          FAILED ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 2");
     END IF;

     IF IDENT (CHARRAY (0)) + CHARRAY (2) NOT IN
             -2.875 .. -2.8125 OR
        CHARRAY (3)  - IDENT (CHARRAY (2)) NOT IN
             2.8125 .. 2.875 THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 3");
     END IF;

     IF +IDENT (CHARRAY (1)) NOT IN -0.375 .. -0.3125 OR
        IDENT (-CHARRAY (2)) NOT IN -0.6875 .. -0.625 THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 3");
     END IF;

     IF CHECK_TYPE (CHARRAY (0) * IDENT (CHARRAY (2))) NOT IN
          -2.4375 .. -2.1875 OR
        CHECK_TYPE (IDENT (CHARRAY (1)) / CHARRAY (3)) NOT IN
          -0.125 .. -0.0625 THEN
          FAILED ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 3");
     END IF;

     IF ABS IDENT (CHARRAY (1)) NOT IN 0.3125 .. 0.375 OR
        IDENT (ABS CHARRAY (2)) NOT IN 0.625 .. 0.6875 THEN
          FAILED ("INCORRECT RESULTS FOR ABSOLUTE VALUE " &
                  "OPERATORS - 3");
     END IF;

     IF IDENT (CHARRAY (2)) NOT IN 0.625 .. 0.6875 OR
            CHARRAY (1) IN -0.25 .. 0.0 OR
            IDENT (CHARRAY (1)) IN -1.0 .. -0.4375 THEN
          FAILED ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 3");
     END IF;

     IF IDENT (CHREC.COMPN1) + CHREC.COMPP1 NOT IN
             -2.875 .. -2.8125 OR
        CHREC.COMPP2  - IDENT (CHREC.COMPP1) NOT IN
             2.8125 .. 2.875 THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 4");
     END IF;

     IF +IDENT (CHREC.COMPN2) NOT IN -0.375 .. -0.3125 OR
        IDENT (-CHREC.COMPP1) NOT IN -0.6875 .. -0.625 THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 4");
     END IF;

     IF CHECK_TYPE (CHREC.COMPN1 * IDENT (CHREC.COMPP1)) NOT IN
          -2.4375 .. -2.1875 OR
        CHECK_TYPE (IDENT (CHREC.COMPN2) / CHREC.COMPP2) NOT IN
          -0.125 .. -0.0625 THEN
          FAILED ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 4");
     END IF;

     IF ABS IDENT (CHREC.COMPN2) NOT IN 0.3125 .. 0.375 OR
        IDENT (ABS CHREC.COMPP1) NOT IN 0.625 .. 0.6875 THEN
          FAILED ("INCORRECT RESULTS FOR ABSOLUTE VALUE " &
                  "OPERATORS - 4");
     END IF;

     IF IDENT (CHREC.COMPP1) NOT IN 0.625 .. 0.6875 OR
           CHREC.COMPN2 IN -0.25 .. 0.0 OR
           IDENT (CHREC.COMPN2) IN -1.0 .. -0.4375 THEN
          FAILED ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 4");
     END IF;

     RESULT;
END CD2D11A;
