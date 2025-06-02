-- CD1C04D.ADA

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
--     CHECK THAT AN ENUMERATION REPRESENTATION CLAUSE CAN BE GIVEN
--     FOR A DERIVED ENUMERATION TYPE EVEN IF THE REPRESENTATION IS
--     INHERITED FROM THE PARENT, AND THAT THE CLAUSE FOR THE DERIVED
--     TYPE OVERRIDES THAT OF THE PARENT.

-- HISTORY:
--     JET 09/21/87  CREATED ORIGINAL TEST.
--     DHH 03/29/89  CHANGE FROM 'A' TEST TO 'C' TEST AND FROM '.DEP'
--                   '.ADA'.  ADDED CHECK ON REPRESENTATION CLAUSE.

WITH REPORT; USE REPORT;
WITH ENUM_CHECK;                      -- CONTAINS A CALL TO 'FAILED'.
PROCEDURE CD1C04D IS

     TYPE NORMAL_TYPE IS (RED, BLUE, GREEN, YELLOW);

     TYPE PARENT_TYPE IS (RED, BLUE, GREEN, YELLOW);

     FOR PARENT_TYPE USE
          (RED => 256, BLUE => 257, GREEN => 258, YELLOW => 259);

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;

     FOR DERIVED_TYPE USE
          (RED => 16, BLUE => 17, GREEN => 18, YELLOW => 19);

     TYPE INT1 IS RANGE 16 .. 19;
     FOR INT1'SIZE USE DERIVED_TYPE'SIZE;

     PROCEDURE CHECK_1 IS NEW ENUM_CHECK(DERIVED_TYPE, INT1);

BEGIN

     TEST("CD1C04D", "CHECK THAT AN ENUMERATION REPRESENTATION " &
                     "CLAUSE CAN BE GIVEN FOR A DERIVED ENUMERATION " &
                     "TYPE EVEN IF THE REPRESENTATION IS INHERITED " &
                     "FROM THE PARENT, AND THAT THE CLAUSE FOR THE " &
                     "DERIVED TYPE OVERRIDES THAT OF THE PARENT");

     IF PARENT_TYPE'SIZE = IDENT_INT (NORMAL_TYPE'SIZE) THEN
          COMMENT ("PARENT_TYPE'SIZE WAS NOT AFFECTED BY THE " &
                   "REPRESENTATION CLAUSE");
     END IF;

     IF DERIVED_TYPE'SIZE >= IDENT_INT (PARENT_TYPE'SIZE) THEN
          COMMENT ("THE SPECIFICATION OF SMALLER VALUES FOR THE " &
                   "REPRESENTATION OF DERIVED_TYPE DID NOT " &
                   "REDUCE THE SIZE OF DERIVED_TYPE");
     END IF;

     CHECK_1 (DERIVED_TYPE'(GREEN), 18, "DERIVED_TYPE");

     RESULT;

END CD1C04D;
