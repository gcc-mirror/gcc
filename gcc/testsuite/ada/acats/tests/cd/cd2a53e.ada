-- CD2A53E.ADA

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
--     CHECK THAT WHEN SIZE AND SMALL SPECIFICATIONS ARE GIVEN FOR A
--     FIXED POINT TYPE, THEN OPERATIONS ON VALUES OF SUCH A TYPE
--     ARE NOT AFFECTED BY THE REPRESENTATION CLAUSE WHEN THE TYPE
--     IS PASSED AS A GENERIC ACTUAL PARAMETER.

-- HISTORY:
--     BCB 08/24/87  CREATED ORIGINAL TEST.
--     DHH 04/12/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND CHANGED
--                   OPERATORS ON 'SIZE TESTS.
--     WMC 04/01/92  ELIMINATED TEST REDUNDANCIES.
--     MRM 07/16/92  FIX ALIGNMENT OF BLOCK BODY
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH REPORT; USE REPORT;
PROCEDURE CD2A53E IS

     BASIC_SIZE : CONSTANT := INTEGER'SIZE/2;
     BASIC_SMALL : CONSTANT := 2.0 ** (-4);
     B : BOOLEAN;

     TYPE CHECK_TYPE IS DELTA 1.0 RANGE -4.0 .. 4.0;
     FOR CHECK_TYPE'SMALL USE BASIC_SMALL;
     FOR CHECK_TYPE'SIZE USE BASIC_SIZE;

BEGIN

     TEST ("CD2A53E", "CHECK THAT WHEN SIZE AND SMALL SPECIFICATIONS " &
                      "ARE GIVEN FOR A FIXED POINT TYPE, THEN " &
                      "OPERATIONS ON VALUES OF SUCH A TYPE ARE NOT " &
                      "AFFECTED BY THE REPRESENTATION CLAUSE WHEN " &
                      "THE TYPE IS PASSED AS A GENERIC ACTUAL " &
                      "PARAMETER");

     DECLARE

          GENERIC

               TYPE FIXED_ELEMENT IS DELTA <>;

          FUNCTION FUNC RETURN BOOLEAN;

          FUNCTION FUNC RETURN BOOLEAN IS

               ZERO  : CONSTANT :=  0.0;

               TYPE BASIC_TYPE IS DELTA 2.0 ** (-4) RANGE -4.0 .. 4.0;

               CNEG1 : FIXED_ELEMENT := -3.5;
               CNEG2 : FIXED_ELEMENT := FIXED_ELEMENT (-1.0/3.0);
               CPOS1 : FIXED_ELEMENT := FIXED_ELEMENT (4.0/6.0);
               CPOS2 : FIXED_ELEMENT :=  3.5;
               CZERO : FIXED_ELEMENT;

               TYPE ARRAY_TYPE IS ARRAY (0 .. 3) OF FIXED_ELEMENT;
               CHARRAY : ARRAY_TYPE :=
                   (-3.5, FIXED_ELEMENT (-1.0/3.0), FIXED_ELEMENT
                    (4.0/6.0), 3.5);

               TYPE REC_TYPE IS RECORD
                    COMPF : FIXED_ELEMENT := -3.5;
                    COMPN : FIXED_ELEMENT := FIXED_ELEMENT (-1.0/3.0);
                    COMPP : FIXED_ELEMENT := FIXED_ELEMENT (4.0/6.0);
                    COMPL : FIXED_ELEMENT :=  3.5;
               END RECORD;

               CHREC : REC_TYPE;

               FUNCTION IDENT (FX : FIXED_ELEMENT) RETURN
                    FIXED_ELEMENT IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RETURN FX;
                    ELSE
                         RETURN 0.0;
                    END IF;
               END IDENT;

               PROCEDURE PROC (CN1IN, CP1IN      :        FIXED_ELEMENT;
                               CN2INOUT,CP2INOUT : IN OUT FIXED_ELEMENT;
                               CZOUT             :    OUT FIXED_ELEMENT)
                               IS
               BEGIN

                    IF +IDENT (CN2INOUT) NOT IN -0.375 .. -0.3125 OR
                        IDENT (-CP1IN) NOT IN -0.6875 .. -0.625 THEN
                        FAILED ("INCORRECT RESULTS FOR " &
                                "UNARY ADDING OPERATORS - 1");
                    END IF;

                    IF ABS IDENT (CN2INOUT) NOT IN 0.3125 .. 0.375 OR
                         IDENT (ABS CP1IN) NOT IN 0.625 .. 0.6875 THEN
                         FAILED ("INCORRECT RESULTS FOR " &
                                 "ABSOLUTE VALUE OPERATORS - 1");
                    END IF;

                    CZOUT := 0.0;

               END PROC;

          BEGIN -- FUNC

               PROC (CNEG1, CPOS1, CNEG2, CPOS2, CZERO);

               IF IDENT (CZERO) /= ZERO THEN
                    FAILED ("INCORRECT VALUE FOR OUT PARAMETER");
               END IF;

               IF FIXED_ELEMENT'LAST < IDENT (3.9375) THEN
                    FAILED ("INCORRECT VALUE FOR FIXED_ELEMENT'LAST");
               END IF;

               IF FIXED_ELEMENT'SIZE /= IDENT_INT (BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR FIXED_ELEMENT'SIZE");
               END IF;

               IF FIXED_ELEMENT'SMALL /= BASIC_SMALL THEN
                    FAILED ("INCORRECT VALUE FOR FIXED_ELEMENT'SMALL");
               END IF;

               IF FIXED_ELEMENT'AFT /= 1 THEN
                    FAILED ("INCORRECT VALUE FOR FIXED_ELEMENT'AFT");
               END IF;

               IF CNEG1'SIZE < IDENT_INT(BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR CNEG1'SIZE");
               END IF;

               IF IDENT (CNEG1) + CPOS1 NOT IN -2.875 .. -2.8125 OR
                   CPOS2  - IDENT (CPOS1) NOT IN 2.8125 .. 2.875 THEN
                    FAILED ("INCORRECT RESULTS FOR BINARY ADDING " &
                            "OPERATORS - 2");
               END IF;

               IF FIXED_ELEMENT (CNEG1 * IDENT (CPOS1)) NOT IN
                    -2.4375 .. -2.1875 OR
                  FIXED_ELEMENT (IDENT (CNEG2) / CPOS2) NOT IN
                    -0.125 .. -0.0625 THEN
                    FAILED ("INCORRECT RESULTS FOR MULTIPLYING " &
                            "OPERATORS - 2");
               END IF;

               IF IDENT (CPOS1) NOT IN 0.625 .. 0.6875 OR
                      CNEG2 IN -0.25 .. 0.0 OR
                      IDENT (CNEG2) IN -1.0 .. -0.4375 THEN
                    FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                            "OPERATORS - 2");
               END IF;

               IF CHARRAY(1)'SIZE < IDENT_INT(BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR CHARRAY(1)'SIZE");
               END IF;

               IF +IDENT (CHARRAY (1)) NOT IN -0.375 .. -0.3125 OR
                    IDENT (-CHARRAY (2)) NOT IN -0.6875 .. -0.625 THEN
                    FAILED ("INCORRECT RESULTS FOR UNARY ADDING " &
                            "OPERATORS - 3");
               END IF;

               IF ABS IDENT (CHARRAY (1)) NOT IN 0.3125 .. 0.375 OR
                  IDENT (ABS CHARRAY (2)) NOT IN 0.625 .. 0.6875 THEN
                    FAILED ("INCORRECT RESULTS FOR ABSOLUTE VALUE " &
                            "OPERATORS - 3");
               END IF;

               IF IDENT (CHARRAY (2)) NOT IN 0.625 .. 0.6875 OR
                      CHARRAY (1) IN -0.25 .. 0.0 OR
                      IDENT (CHARRAY (1)) IN -1.0 .. -0.4375 THEN
                    FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                            "OPERATORS - 3");
               END IF;

               IF CHREC.COMPP'SIZE < IDENT_INT(BASIC_SIZE) THEN
                    FAILED ("INCORRECT VALUE FOR CHREC.COMPP'SIZE");
               END IF;

               IF IDENT (CHREC.COMPF) + CHREC.COMPP NOT IN
                     -2.875 .. -2.8125 OR
                    CHREC.COMPL  - IDENT (CHREC.COMPP) NOT IN
                     2.8125 .. 2.875 THEN
                    FAILED ("INCORRECT RESULTS FOR BINARY ADDING " &
                               "OPERATORS - 4");
               END IF;

               IF FIXED_ELEMENT (CHREC.COMPF * IDENT (CHREC.COMPP))
                    NOT IN -2.4375 .. -2.1875 OR
                  FIXED_ELEMENT (IDENT (CHREC.COMPN) / CHREC.COMPL)
                    NOT IN -0.125 .. -0.0625 THEN
                    FAILED ("INCORRECT RESULTS FOR MULTIPLYING " &
                            "OPERATORS - 4");
               END IF;

               IF IDENT (CHREC.COMPP) NOT IN 0.625 .. 0.6875 OR
                      CHREC.COMPN IN -0.25 .. 0.0 OR
                      IDENT (CHREC.COMPN) IN -1.0 .. -0.4375 THEN
                    FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                            "OPERATORS - 4");
               END IF;

               RETURN TRUE;

          END FUNC;

          FUNCTION NEWFUNC IS NEW FUNC(CHECK_TYPE);
     BEGIN
          B := NEWFUNC;
     END;

     RESULT;

END CD2A53E;
