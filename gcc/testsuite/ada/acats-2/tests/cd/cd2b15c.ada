-- CD2B15C.ADA

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
--     IF THE COLLECTION SIZE IS LARGE ENOUGH TO HOLD SOME
--     VALUES OF THE DESIGNATED TYPE, CHECK THAT "STORAGE_ERROR"
--     IS RAISED BY AN ALLOCATOR WHEN INSUFFICIENT STORAGE IS
--     AVAILABLE.

-- HISTORY:
--     DHH 09/23/87 CREATED ORIGINAL TEST.
--     PMW 09/19/88 MODIFIED WITHDRAWN TEST.
--     THS 03/21/90 CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND
--                  COMPLETELY REVISED THE TEST TO PREVENT OPTIMIZATION.
--     LDC 09/20/90 REMOVED UNUSED VARIABLE, CHANGED FAIL CALLS TO 
--                  COMMENT FOR 'STORAGE_SIZE /= TO SPECIFIED SIZE,
--                  MOVED LOOP FOR CHECK VALUES TO EXCEPTION HANDLER.

WITH REPORT; USE REPORT;
WITH SYSTEM;
PROCEDURE CD2B15C IS

     SPECIFIED_SIZE : CONSTANT := 1000;

     TYPE CHECK_TYPE IS ACCESS INTEGER;
     FOR CHECK_TYPE'STORAGE_SIZE USE SPECIFIED_SIZE;

     UNITS_PER_INTEGER : CONSTANT :=
         (INTEGER'SIZE + SYSTEM.STORAGE_UNIT - 1) / SYSTEM.STORAGE_UNIT;

     TYPE ACC_ARRAY_TYPE IS ARRAY
         (INTEGER RANGE 1 .. (CHECK_TYPE'STORAGE_SIZE /
          UNITS_PER_INTEGER) + 1) OF CHECK_TYPE;
     ACC_ARRAY : ACC_ARRAY_TYPE;

     PLACE_I_STOPPED : INTEGER := 0;

BEGIN

     TEST ("CD2B15C", "IF THE COLLECTION SIZE IS LARGE " &
                      "ENOUGH TO HOLD SOME VALUES OF " &
                      "THE DESIGNATED TYPE, CHECK THAT " &
                      "STORAGE_ERROR IS RAISED BY AN " &
                      "ALLOCATOR WHEN INSUFFICIENT STORAGE " &
                      "IS AVAILABLE");

     IF CHECK_TYPE'STORAGE_SIZE < IDENT_INT (SPECIFIED_SIZE) THEN
          FAILED ("CHECK_TYPE'STORAGE_SIZE IS LESS THEN THE VALUE " &
                  "SPECIFIED IN THE REPRESENTATION CLAUSE");

     ELSIF CHECK_TYPE'STORAGE_SIZE > 2 * IDENT_INT (SPECIFIED_SIZE) THEN
          COMMENT ("VALUE FOR CHECK_TYPE'STORAGE_SIZE IS MORE THEN " &
                   "TWICE THE SPECIFIED VALUE IN THE REPRESENTATION " &
                   "CLAUSE");
     END IF;

     BEGIN

          FOR I IN ACC_ARRAY'RANGE LOOP
               ACC_ARRAY (I) := NEW INTEGER'(IDENT_INT (I));
               PLACE_I_STOPPED := I;
          END LOOP;

          FAILED ("NO EXCEPTION RAISED WHEN RESERVED SPACE " &
                  "EXCEEDED");

     EXCEPTION
          WHEN STORAGE_ERROR =>
               FOR I IN 1 .. PLACE_I_STOPPED LOOP
                    IF ACC_ARRAY (I).ALL /= IDENT_INT (I) THEN
                        FAILED ("INCORRECT VALUE FOR ACC_ARRAY (" &
                                INTEGER'IMAGE (I) & ")");
                    END IF;
               END LOOP;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED WHEN RESERVED SPACE " &
                       "EXCEEDED");
     END;

     RESULT;

END CD2B15C;
