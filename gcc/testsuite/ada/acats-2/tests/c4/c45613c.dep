-- C45613C.DEP

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED
--     BY "**" FOR LONG_INTEGER WHEN THE RESULT EXCEEDS THE RANGE
--     OF THE BASE TYPE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE TO THOSE IMPLEMENTATIONS WHICH SUPPORT
--     LONG_INTEGER.

--     IF "LONG_INTEGER" IS NOT SUPPORTED, THEN THE DECLARATION OF
--     "CHECK_LONG" MUST BE REJECTED.


-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- HISTORY:
--     HTG 10/06/86 CREATED ORIGINAL TEST.
--     DHH 01/13/88 ADDED APPLICABILITY CRITERIA AND STANDARD HEADER.
--     MRM 03/30/93 REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
PROCEDURE C45613C IS

     CHECK_LONG : LONG_INTEGER;                      -- N/A => ERROR.

     FUNCTION IDENT (X : LONG_INTEGER) RETURN LONG_INTEGER IS
     BEGIN
          RETURN LONG_INTEGER (IDENT_INT (INTEGER (X)));
     END IDENT;

BEGIN
     TEST ("C45613C","CHECK THAT CONSTRAINT_ERROR " &
                     "IS RAISED BY ""**"" FOR LONG_INTEGER WHEN " &
                     "THE RESULT EXCEEDS THE RANGE OF THE BASE TYPE");

     DECLARE
          INT : LONG_INTEGER;
     BEGIN
          INT := IDENT(LONG_INTEGER'LAST ** IDENT_INT(2));
          FAILED ("NO EXCEPTION FOR SECOND POWER OF " &
                  "LONG_INTEGER'LAST");

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR " &
                            "SECOND POWER OF " &
                            "LONG_INTEGER'LAST");
     END;

     DECLARE
          INT : LONG_INTEGER;
     BEGIN
          INT := IDENT(LONG_INTEGER'FIRST ** IDENT_INT(3));
                 FAILED ("NO EXCEPTION FOR THIRD POWER OF " &
                         "LONG_INTEGER'FIRST");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED FOR " &
                            "THIRD POWER OF " &
                            "LONG_INTEGER'FIRST");

     END;

     RESULT;

END C45613C;
