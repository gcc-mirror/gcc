-- CD1C03A.ADA

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
--     CHECK THAT THE SIZE OF A DERIVED TYPE IS INHERITED FROM THE
--     PARENT IF THE SIZE OF THE PARENT WAS DETERMINED BY A SIZE
--     CLAUSE.

-- HISTORY:
--     JET  09/16/87 CREATED ORIGINAL TEST.
--     DHH  03/30/89 CHANGED SPECIFIED_SIZE TO 5, ADDED CHECK ON
--                   REPRESENTATION CLAUSES, AND CHANGED THE TEST
--                   EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT; USE REPORT;
WITH LENGTH_CHECK;                     -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD1C03A IS

     SPECIFIED_SIZE : CONSTANT := 5;

     TYPE PARENT_TYPE IS RANGE -8 .. 7;

     FOR PARENT_TYPE'SIZE USE SPECIFIED_SIZE;
     PT : PARENT_TYPE := -7;

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;

     DT : DERIVED_TYPE := -7;
     PROCEDURE CHECK_1 IS NEW LENGTH_CHECK (DERIVED_TYPE);
     PROCEDURE CHECK_2 IS NEW LENGTH_CHECK (PARENT_TYPE);

BEGIN

     TEST("CD1C03A", "CHECK THAT THE SIZE OF A DERIVED TYPE IS " &
                     "INHERITED FROM THE PARENT IF THE SIZE OF " &
                     "THE PARENT WAS DETERMINED BY A SIZE CLAUSE");

     IF PARENT_TYPE'SIZE /= IDENT_INT (SPECIFIED_SIZE) THEN
          FAILED ("PARENT_TYPE'SIZE /= " &
                  INTEGER'IMAGE(SPECIFIED_SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(PARENT_TYPE'SIZE));
     END IF;

     IF DERIVED_TYPE'SIZE /= IDENT_INT (SPECIFIED_SIZE) THEN
          FAILED ("DERIVED_TYPE'SIZE /= " &
                  INTEGER'IMAGE(SPECIFIED_SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(DERIVED_TYPE'SIZE));
     END IF;

     IF DT'SIZE < IDENT_INT (SPECIFIED_SIZE) THEN
          FAILED ("DT'SIZE SHOULD NOT BE LESS THAN" &
                  INTEGER'IMAGE(SPECIFIED_SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(DT'SIZE));
     END IF;

     CHECK_1 (DT, 5, "DERIVED_TYPE");
     CHECK_2 (PT, 5, "PARENT_TYPE");
     RESULT;

END CD1C03A;
