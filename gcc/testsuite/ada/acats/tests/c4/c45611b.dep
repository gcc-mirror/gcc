--  C45611B.DEP

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
--     CHECK THAT EXPONENTIATION OF A SHORT_INTEGER TO AN INTEGER VALUE
--     IS CORRECTLY EVALUATED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     SHORT_INTEGER.

--     IF "SHORT_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_SHORT" MUST BE REJECTED.

-- HISTORY:
--     HTG 09/23/86 CREATED ORIGINAL TEST.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.

WITH REPORT; USE REPORT;

PROCEDURE C45611B IS

    CHECK_SHORT : SHORT_INTEGER;                   -- N/A => ERROR.

    I1,INT : SHORT_INTEGER;

    FUNCTION IDENT (X : SHORT_INTEGER) RETURN SHORT_INTEGER IS
    BEGIN
         RETURN SHORT_INTEGER (IDENT_INT (INTEGER (X)));
    END IDENT;

    BEGIN


         TEST ("C45611B", "CHECK THAT EXPONENTIATION OF A " &
                          "SHORT_INTEGER VALUE IS CORRECTLY " &
                          "EVALUATED");

         I1 := IDENT(0) ** IDENT_INT(0);

         IF IDENT(I1) /= IDENT(1) THEN
              FAILED( "INCORRECT RESULT FOR '0**0'" );
         END IF;

         INT := "**" (IDENT(0),IDENT_INT(1));

         IF IDENT(INT) /= IDENT(0) THEN
              FAILED( "INCORRECT RESULT FOR '0**1'" );
         END IF;

         I1 := IDENT(6) ** IDENT_INT(0);

         IF IDENT(I1) /= IDENT(1) THEN
              FAILED( "INCORRECT RESULT FOR '6**0'" );
         END IF;

         INT := IDENT(15) ** IDENT_INT(1);

         IF IDENT(INT) /= IDENT(15) THEN
              FAILED( "INCORRECT RESULT FOR '15**1'" );
         END IF;

         I1 := IDENT(-3) ** IDENT_INT(0);

         IF IDENT(I1) /= IDENT(1) THEN
              FAILED( "INCORRECT RESULT FOR '(-3)**0'" );
         END IF;

         INT := "**" (IDENT(-7),IDENT_INT(1));

         IF IDENT(INT) /= IDENT(-7) THEN
              FAILED( "INCORRECT RESULT FOR '(-7)**1'" );
         END IF;

         I1 := "**" (IDENT(-1),IDENT_INT(2));

         IF IDENT(I1) /= IDENT(1) THEN
              FAILED( "INCORRECT RESULT FOR '(-1)**2'" );
         END IF;


         INT := IDENT(-1) ** IDENT_INT(3);

         IF IDENT(INT) /= IDENT(-1) THEN
              FAILED( "INCORRECT RESULT FOR '(-1)**3'" );
         END IF;

         INT := "**" (IDENT(0),IDENT_INT(2));

         IF IDENT(INT) /= IDENT(0) THEN
              FAILED( "INCORRECT RESULT FOR '0**2'" );
         END IF;

         INT := IDENT(0) ** IDENT_INT(10);

         IF IDENT(INT) /= IDENT(0) THEN
              FAILED( "INCORRECT RESULT FOR '0**10'" );
         END IF;

         INT := "**" (IDENT(6),IDENT_INT(2));

         IF IDENT(INT) /= IDENT(36) THEN
              FAILED( "INCORRECT RESULT FOR '6**2'" );
         END IF;

         INT := "**" (IDENT(2),IDENT_INT(2));

         IF IDENT(INT) /= IDENT(4) THEN
              FAILED( "INCORRECT RESULT FOR '2**2'" );
         END IF;

         I1 := "**" (IDENT(1),IDENT_INT(10));

         IF IDENT(I1) /= IDENT(1) THEN
              FAILED( "INCORRECT RESULT FOR '1**10'" );
         END IF;

         RESULT;

    END C45611B;
