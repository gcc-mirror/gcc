-- CD1C03I.ADA

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
--     CHECK THAT THE RECORD SIZE AND THE COMPONENT POSITIONS AND
--     SIZES OF A DERIVED RECORD TYPE ARE INHERITED FROM THE
--     PARENT IF THOSE ASPECTS OF THE PARENT WERE DETERMINED BY THE
--     PRAGMA PACK.

-- HISTORY:
--     JET 09/17/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE CD1C03I IS

     TYPE E_TYPE IS (RED, BLUE, GREEN);

     TYPE PARENT_TYPE IS
          RECORD
               B1: BOOLEAN := TRUE;
               I : INTEGER RANGE 0 .. 127 := 127;
               C : CHARACTER := 'S';
               B2: BOOLEAN := FALSE;
               E : E_TYPE := BLUE;
          END RECORD;

     PRAGMA PACK (PARENT_TYPE);

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;

     P_REC : PARENT_TYPE;
     REC   : DERIVED_TYPE;

BEGIN

     TEST("CD1C03I", "CHECK THAT THE RECORD SIZE AND THE COMPONENT " &
                     "POSITIONS AND SIZES OF A DERIVED RECORD " &
                     "TYPE ARE INHERITED FROM THE PARENT IF THOSE " &
                     "ASPECTS OF THE PARENT WERE DETERMINED BY " &
                     "THE PRAGMA PACK");

     IF DERIVED_TYPE'SIZE /= PARENT_TYPE'SIZE THEN
          FAILED ("DERIVED_TYPE'SIZE WAS NOT INHERITED FROM " &
                  "PARENT_TYPE");
     END IF;

     IF REC.I'SIZE /= P_REC.I'SIZE OR
        REC.C'SIZE /= P_REC.C'SIZE OR
        REC.B1'SIZE /= P_REC.B1'SIZE OR
        REC.B2'SIZE /= P_REC.B2'SIZE OR
        REC.E'SIZE /= P_REC.E'SIZE THEN
          FAILED ("THE SIZES OF DERIVED_TYPE ELEMENTS WERE NOT " &
                  "INHERITED FROM PARENT_TYPE");
     END IF;

     REC := (FALSE, 12, 'T', TRUE, RED);

     IF (REC.I /= 12) OR (REC.C /= 'T') OR
         REC.B1 OR (NOT REC.B2) OR (REC.E /= RED) THEN
          FAILED ("THE VALUES OF DERIVED_TYPE COMPONENTS WERE " &
                  "INCORRECT");
     END IF;

     IF REC.I'POSITION /= P_REC.I'POSITION OR
        REC.C'POSITION /= P_REC.C'POSITION OR
        REC.B1'POSITION /= P_REC.B1'POSITION OR
        REC.B2'POSITION /= P_REC.B2'POSITION OR
        REC.E'POSITION /= P_REC.E'POSITION THEN
          FAILED ("THE POSITIONS OF DERIVED_TYPE COMPONENTS WERE " &
                  "NOT INHERITED FROM PARENT_TYPE");
     END IF;

     IF REC.I'FIRST_BIT /= P_REC.I'FIRST_BIT OR
        REC.C'FIRST_BIT /= P_REC.C'FIRST_BIT OR
        REC.B1'FIRST_BIT /= P_REC.B1'FIRST_BIT OR
        REC.B2'FIRST_BIT /= P_REC.B2'FIRST_BIT OR
        REC.E'FIRST_BIT /= P_REC.E'FIRST_BIT THEN
          FAILED ("THE FIRST_BITS OF DERIVED_TYPE COMPONENTS WERE " &
                  "NOT INHERITED FROM PARENT_TYPE");
     END IF;

     IF REC.I'LAST_BIT /= P_REC.I'LAST_BIT OR
        REC.C'LAST_BIT /= P_REC.C'LAST_BIT OR
        REC.B1'LAST_BIT /= P_REC.B1'LAST_BIT OR
        REC.B2'LAST_BIT /= P_REC.B2'LAST_BIT OR
        REC.E'LAST_BIT /= P_REC.E'LAST_BIT THEN
          FAILED ("THE LAST_BITS OF DERIVED_TYPE COMPONENTS WERE " &
                  "NOT INHERITED FROM PARENT_TYPE");
     END IF;

     RESULT;

END CD1C03I;
