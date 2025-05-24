-- CD1C04E.ADA

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
--     CHECK THAT A RECORD REPRESENTATION CLAUSE CAN BE GIVEN FOR
--     A DERIVED RECORD TYPE EVEN IF THE REPRESENTATION IS INHERITED
--     FROM THE PARENT, AND THAT THE REPRESENTATION CLAUSE FOR THE
--     DERIVED TYPE OVERRIDES THAT OF THE PARENT TYPE.

-- HISTORY:
--     PWB 03/25/89  DELETED CHECKS OF COMPONENT'SIZE; CHANGED
--         EXTENSION FROM '.ADA' TO '.DEP'.
--     JET 09/21/87  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
WITH SYSTEM; USE SYSTEM;

PROCEDURE CD1C04E IS

     UNITS_PER_INTEGER : CONSTANT :=
          (INTEGER'SIZE + SYSTEM.STORAGE_UNIT - 1) /
          SYSTEM.STORAGE_UNIT;

     TYPE E_TYPE IS (RED, BLUE, GREEN);

     TYPE PARENT_TYPE IS
          RECORD
               I : INTEGER RANGE 0 .. 127 := 127;
               C : CHARACTER := 'S';
               B : BOOLEAN := FALSE;
               E : E_TYPE := BLUE;
          END RECORD;

     FOR PARENT_TYPE USE
          RECORD
               C AT 0 * UNITS_PER_INTEGER RANGE 0 .. CHARACTER'SIZE - 1;
               B AT 1 * UNITS_PER_INTEGER RANGE 0 .. BOOLEAN'SIZE - 1;
               I AT 2 * UNITS_PER_INTEGER RANGE 0 .. INTEGER'SIZE/2 - 1;
               E AT 3 * UNITS_PER_INTEGER RANGE 0 .. CHARACTER'SIZE - 1;
          END RECORD;

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;

     FOR DERIVED_TYPE USE
          RECORD
               C AT 1 * UNITS_PER_INTEGER RANGE 1 .. CHARACTER'SIZE + 1;
               B AT 3 * UNITS_PER_INTEGER RANGE 1 .. BOOLEAN'SIZE + 1;
               I AT 5 * UNITS_PER_INTEGER RANGE 1 .. INTEGER'SIZE/2 + 1;
               E AT 7 * UNITS_PER_INTEGER RANGE 1 .. CHARACTER'SIZE + 1;
          END RECORD;

     P_REC : PARENT_TYPE;
     REC   : DERIVED_TYPE;

BEGIN

     TEST("CD1C04E", "CHECK THAT A RECORD REPRESENTATION CLAUSE " &
                     "CAN BE GIVEN FOR A DERIVED RECORD TYPE EVEN " &
                     "IF THE REPRESENTATION IS INHERITED FROM " &
                     "THE PARENT, AND THAT THE REPRESENTATION " &
                     "CLAUSE FOR THE DERIVED TYPE OVERRIDES THAT " &
                     "OF THE PARENT TYPE");

     IF DERIVED_TYPE'SIZE = IDENT_INT (PARENT_TYPE'SIZE) THEN
          FAILED ("DERIVED_TYPE'SIZE WAS INHERITED FROM " &
                  "PARENT_TYPE");
     END IF;

     REC := (12, 'T', TRUE, RED);

     IF (REC.I /= 12) OR (REC.C /= 'T') OR
        (NOT REC.B) OR (REC.E /= RED) THEN
          FAILED ("THE VALUES OF DERIVED_TYPE COMPONENTS WERE " &
                  "INCORRECT");
     END IF;

     IF REC.I'POSITION = P_REC.I'POSITION OR
        REC.C'POSITION = P_REC.C'POSITION OR
        REC.B'POSITION = P_REC.B'POSITION OR
        REC.E'POSITION = P_REC.E'POSITION THEN
          FAILED ("THE POSITIONS OF DERIVED_TYPE COMPONENTS WERE " &
                  "INHERITED FROM PARENT_TYPE");
     END IF;

     IF REC.I'FIRST_BIT = P_REC.I'FIRST_BIT OR
        REC.C'FIRST_BIT = P_REC.C'FIRST_BIT OR
        REC.B'FIRST_BIT = P_REC.B'FIRST_BIT OR
        REC.E'FIRST_BIT = P_REC.E'FIRST_BIT THEN
          FAILED ("THE FIRST_BITS OF DERIVED_TYPE COMPONENTS WERE " &
                  "INHERITED FROM PARENT_TYPE");
     END IF;

     IF REC.I'LAST_BIT = P_REC.I'LAST_BIT OR
        REC.C'LAST_BIT = P_REC.C'LAST_BIT OR
        REC.B'LAST_BIT = P_REC.B'LAST_BIT OR
        REC.E'LAST_BIT = P_REC.E'LAST_BIT THEN
          FAILED ("THE LAST_BITS OF DERIVED_TYPE COMPONENTS WERE " &
                  "INHERITED FROM PARENT_TYPE");
     END IF;

     RESULT;

END CD1C04E;
