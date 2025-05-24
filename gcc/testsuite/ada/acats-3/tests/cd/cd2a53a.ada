-- CD2A53A.ADA

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
--     FIXED POINT TYPE, THEN OPERATIONS ON VALUES OF SUCH A TYPE ARE
--     NOT AFFECTED BY THE REPRESENTATION CLAUSE.

-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C)
--          and which support decimal small values:
--          The test must compile, bind, execute, report PASSED, and
--          complete normally.
--
--      For other implementations:
--          This test may produce at least one error message at compilation,
--          and the error message is associated with one of the items marked:
--             -- N/A => ERROR.
--          The test will be recorded as Not_Applicable.
--          Otherwise, the test must execute and report PASSED.
--
--      All other behaviors are FAILING.
--
-- HISTORY:
--     BCB 08/24/87  CREATED ORIGINAL TEST.
--     DHH 04/12/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   OPERATORS ON 'SIZE TESTS, AND CHANGED 'SIZE CLAUSE
--                   SO THAT IT IS NOT A POWER OF TWO.
--     WMC 04/01/92  ELIMINATED TEST REDUNDANCIES.
--     RLB 11/24/98  Added Ada 95 applicability criteria.

WITH REPORT; USE REPORT;
PROCEDURE CD2A53A IS
     BASIC_SIZE : CONSTANT := 15;
     BASIC_SMALL : CONSTANT := 0.01;

     ZERO  : CONSTANT :=  0.0;

     TYPE CHECK_TYPE IS DELTA 1.0 RANGE -4.0 .. 4.0;

     FOR CHECK_TYPE'SMALL USE BASIC_SMALL;   -- N/A => ERROR.
     FOR CHECK_TYPE'SIZE USE BASIC_SIZE;     -- N/A => ERROR.

     CNEG1 : CHECK_TYPE := -2.7;
     CNEG2 : CHECK_TYPE := CHECK_TYPE (-1.0/3.0);
     CPOS1 : CHECK_TYPE := CHECK_TYPE (4.0/6.0);
     CPOS2 : CHECK_TYPE :=  2.7;
     CZERO : CHECK_TYPE;

     TYPE ARRAY_TYPE IS ARRAY (0 .. 3) OF CHECK_TYPE;
     CHARRAY : ARRAY_TYPE :=
          (-2.7, CHECK_TYPE (-1.0/3.0), CHECK_TYPE (4.0/6.0), 2.7);

     TYPE REC_TYPE IS RECORD
          COMPF : CHECK_TYPE := -2.7;
          COMPN : CHECK_TYPE := CHECK_TYPE (-1.0/3.0);
          COMPP : CHECK_TYPE := CHECK_TYPE (4.0/6.0);
          COMPL : CHECK_TYPE :=  2.7;
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

     PROCEDURE PROC (CN1IN, CP1IN      :        CHECK_TYPE;
                     CN2INOUT,CP2INOUT : IN OUT CHECK_TYPE;
                     CZOUT             :    OUT CHECK_TYPE) IS
     BEGIN

          IF IDENT (CN1IN) + CP1IN NOT IN -2.04 .. -2.03 OR
             CP2INOUT  - IDENT (CP1IN) NOT IN 2.03 .. 2.04 THEN
               FAILED ("INCORRECT RESULTS FOR " &
                       "BINARY ADDING OPERATORS - 1");
          END IF;

          IF CHECK_TYPE (CN1IN * IDENT (CP1IN)) NOT IN
             -1.81 .. -1.78 OR
             CHECK_TYPE (IDENT (CN2INOUT) / CP2INOUT) NOT IN
             -0.13 .. -0.12 THEN
               FAILED ("INCORRECT RESULTS FOR " &
                       "MULTIPLYING OPERATORS - 1");
          END IF;

          IF IDENT (CP1IN) NOT IN 0.66 .. 0.670 OR
                 CN2INOUT IN -0.32 .. 0.0 OR
                 IDENT (CN2INOUT) IN -1.0 .. -0.35 THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                       "OPERATORS - 1");
          END IF;

          CZOUT := 0.0;

     END PROC;

BEGIN
     TEST ("CD2A53A", "CHECK THAT WHEN SIZE AND SMALL SPECIFICATIONS " &
                      "ARE GIVEN FOR A FIXED POINT TYPE, THEN " &
                      "OPERATIONS ON VALUES OF SUCH A TYPE ARE NOT " &
                      "AFFECTED BY THE REPRESENTATION CLAUSE");

     PROC (CNEG1, CPOS1, CNEG2, CPOS2, CZERO);

     IF CNEG1'SIZE < IDENT_INT(BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR CNEG1'SIZE");
     END IF;

     IF IDENT (CZERO) /= ZERO THEN
          FAILED ("INCORRECT VALUE FOR OUT PARAMETER");
     END IF;

     IF CHECK_TYPE'FIRST > IDENT (-3.99) THEN
          FAILED ("INCORRECT VALUE FOR CHECK_TYPE'FIRST");
     END IF;

     IF CHECK_TYPE'SIZE /= IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
     END IF;

     IF CHECK_TYPE'SMALL /= BASIC_SMALL THEN
          FAILED ("INCORRECT VALUE FOR CHECK_TYPE'SMALL");
     END IF;

     IF CHECK_TYPE'FORE /= 2 THEN
          FAILED ("INCORRECT VALUE FOR CHECK_TYPE'FORE");
     END IF;

     IF +IDENT (CNEG2) NOT IN -0.34 .. -0.33 OR
        IDENT (-CPOS1) NOT IN -0.67 .. -0.66 THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 2");
     END IF;

     IF ABS IDENT (CNEG2) NOT IN 0.33 .. 0.34 OR
        IDENT (ABS CPOS1) NOT IN 0.66 .. 0.670 THEN
          FAILED ("INCORRECT RESULTS FOR ABSOLUTE VALUE " &
                  "OPERATORS - 2");
     END IF;

     IF CHARRAY(1)'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR CHARRAY(1)'SIZE");
     END IF;

     IF IDENT (CHARRAY (0)) + CHARRAY (2) NOT IN
             -2.04 .. -2.03 OR
        CHARRAY (3)  - IDENT (CHARRAY (2)) NOT IN
             2.03 .. 2.04 THEN
          FAILED ("INCORRECT RESULTS FOR BINARY ADDING OPERATORS - 3");
     END IF;

     IF CHECK_TYPE (CHARRAY (0) * IDENT (CHARRAY (2))) NOT IN
          -1.81 .. -1.78 OR
        CHECK_TYPE (IDENT (CHARRAY (1)) / CHARRAY (3)) NOT IN
          -0.13 .. -0.12 THEN
          FAILED ("INCORRECT RESULTS FOR MULTIPLYING OPERATORS - 3");
     END IF;

     IF IDENT (CHARRAY (2)) NOT IN 0.66 .. 0.670 OR
            CHARRAY (1) IN -0.32 .. 0.0 OR
            IDENT (CHARRAY (1)) IN -1.0 .. -0.35 THEN
          FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                  "OPERATORS - 3");
     END IF;

     IF CHREC.COMPP'SIZE < IDENT_INT (BASIC_SIZE) THEN
          FAILED ("INCORRECT VALUE FOR CHREC.COMPP'SIZE");
     END IF;

     IF +IDENT (CHREC.COMPN) NOT IN -0.34 .. -0.33 OR
        IDENT (-CHREC.COMPP) NOT IN -0.67 .. -0.66 THEN
          FAILED ("INCORRECT RESULTS FOR UNARY ADDING OPERATORS - 4");
     END IF;

     IF ABS IDENT (CHREC.COMPN) NOT IN 0.33 .. 0.34 OR
        IDENT (ABS CHREC.COMPP) NOT IN 0.66 .. 0.670 THEN
          FAILED ("INCORRECT RESULTS FOR ABSOLUTE VALUE " &
                  "OPERATORS - 4");
     END IF;

     IF IDENT (CHREC.COMPP) NOT IN 0.66 .. 0.670 OR
            CHREC.COMPN IN -0.32 .. 0.0 OR
            IDENT (CHREC.COMPN) IN -1.0 .. -0.35 THEN
          FAILED ("INCORRECT RESULTS FOR MEMBERSHIP " &
                  "OPERATORS - 4");
     END IF;

     RESULT;

END CD2A53A;
