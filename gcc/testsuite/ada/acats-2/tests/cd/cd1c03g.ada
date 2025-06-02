-- CD1C03G.ADA

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
--     CHECK THAT THE SIZE OF A DERIVED ENUMERATION TYPE IS
--     INHERITED FROM THE PARENT IF THE SIZE OF THE PARENT WAS
--     DETERMINED BY AN ENUMERATION REPRESENTATION CLAUSE.

-- HISTORY:
--     BCB 03/20/89  CHANGED EXTENSION FROM '.ADA' TO '.DEP'.
--     JET 09/17/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE CD1C03G IS

     TYPE NORMAL_TYPE IS (RED, BLUE, GREEN, YELLOW);

     TYPE PARENT_TYPE IS (RED, BLUE, GREEN, YELLOW);

     FOR PARENT_TYPE USE
          (RED => 256, BLUE => 257, GREEN => 258, YELLOW => 259);

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;

BEGIN

     TEST("CD1C03G", "CHECK THAT THE SIZE OF A DERIVED ENUMERATION " &
                     "TYPE IS INHERITED FROM THE PARENT IF THE " &
                     "SIZE OF THE PARENT WAS DETERMINED BY AN " &
                     "ENUMERATION REPRESENTATION CLAUSE");

     IF PARENT_TYPE'SIZE = IDENT_INT (NORMAL_TYPE'SIZE) THEN
          COMMENT ("PARENT_TYPE'SIZE WAS NOT AFFECTED BY THE " &
                   "REPRESENTATION CLAUSE");
     END IF;

     IF DERIVED_TYPE'SIZE /= IDENT_INT (PARENT_TYPE'SIZE) THEN
          FAILED ("DERIVED_TYPE'SIZE WAS NOT INHERITED FROM " &
                  "PARENT_TYPE");
     END IF;

     RESULT;

END CD1C03G;
