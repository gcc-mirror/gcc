--  C45614B.DEP

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED BY PREDEFINED SHORT_INTEGER
--     "**" IF THE SECOND OPERAND HAS A NEGATIVE VALUE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     SHORT_INTEGER.

--     IF "SHORT_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_SHORT" MUST BE REJECTED.

-- HISTORY:
--     HTG 10/07/86 CREATED ORIGINAL TEST.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.

WITH REPORT; USE REPORT;
PROCEDURE C45614B IS

     CHECK_SHORT : SHORT_INTEGER;                   -- N/A => ERROR.

     FUNCTION IDENT (X : SHORT_INTEGER) RETURN SHORT_INTEGER IS
     BEGIN
          RETURN SHORT_INTEGER (IDENT_INT (INTEGER (X)));
     END IDENT;

BEGIN

     TEST ("C45614B", "CHECK THAT CONSTRAINT_ERROR IS RAISED BY " &
                      "PREDEFINED SHORT_INTEGER ""**"" IF THE " &
                      "SECOND OPERAND HAS A NEGATIVE VALUE");

     DECLARE
          A : INTEGER := -2;
          B : SHORT_INTEGER := 3;
          INT : SHORT_INTEGER := 0;
     BEGIN
          INT := IDENT(B ** IDENT_INT(A));
          FAILED ("NO EXCEPTION FOR '3**(-2)'");

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR '3**(-2)'");
     END;

     DECLARE
          A : INTEGER := -3;
          B : SHORT_INTEGER := -5;
          INT : SHORT_INTEGER := 0;
     BEGIN
          INT := IDENT(B ** IDENT_INT(A));
          FAILED ("NO EXCEPTION FOR '(-5)**(-3)'");

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR '(-5)**(-3)'");
     END;

     DECLARE
          B : SHORT_INTEGER := 0;
          INT : SHORT_INTEGER := 0;
     BEGIN
          INT := IDENT(B ** IDENT_INT(-3));
          FAILED ("NO EXCEPTION FOR '0**(-3)");

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR '0**(-3)'");
     END;

     DECLARE
          INT : SHORT_INTEGER := 0;
     BEGIN
          INT := IDENT(-10 ** IDENT_INT(-2));
          FAILED ("NO EXCEPTION FOR '(-10)**(-2)'");

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR '(-10)**(-2)'");
     END;

     DECLARE
          INT : SHORT_INTEGER := 0;
     BEGIN
          INT := IDENT(6 ** IDENT_INT(-4));
          FAILED ("NO EXCEPTION FOR '6**(-4)'");

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR '6**(-4)'");
     END;

     RESULT;

END C45614B;
