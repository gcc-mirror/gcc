-- C67005D.ADA

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
-- CHECK THAT EQUALITY CAN BE REDEFINED FOR AN ARBITRARY TYPE BY USING A
-- SEQUENCE OF RENAMING DECLARATIONS.

-- JBG 9/11/84

WITH REPORT; USE REPORT;
PROCEDURE C67005D IS

     FUNCTION MY_EQUALS  (L, R : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN FALSE;
     END MY_EQUALS;

     GENERIC
          TYPE LP IS LIMITED PRIVATE;
          WITH FUNCTION "=" (L, R : LP) RETURN BOOLEAN;
     PACKAGE EQUALITY_OPERATOR IS
          PACKAGE INNER IS
               FUNCTION "=" (L, R : LP) RETURN BOOLEAN RENAMES
                             EQUALITY_OPERATOR."=";
          END INNER;
     END EQUALITY_OPERATOR;

BEGIN
     TEST ("C67005D", "CHECK REDEFINITION OF ""="" BY RENAMING");

     DECLARE

          CHK1 : BOOLEAN := 3 = IDENT_INT(3);     -- PREDEFINED "="

          -- REDEFINE INTEGER "=".

          PACKAGE INT_EQUALITY IS NEW
               EQUALITY_OPERATOR (INTEGER, MY_EQUALS);
          FUNCTION "=" (L, R : INTEGER) RETURN BOOLEAN RENAMES
               INT_EQUALITY.INNER."=";

          CHK2 : BOOLEAN := 3 = IDENT_INT(3);     -- REDEFINED "=".

     BEGIN

          IF NOT CHK1 THEN
               FAILED ("PREDEFINED ""="" NOT USED");
          END IF;

          IF CHK2 THEN
               FAILED ("REDEFINED ""="" NOT USED");
          END IF;

     END;

     RESULT;

END C67005D;
