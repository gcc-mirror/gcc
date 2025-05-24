-- CD4041A.TST

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
--     CHECK THAT AN ALIGNMENT CLAUSE CAN BE GIVEN FOR A RECORD
--     REPRESENTATION CLAUSE.

-- HISTORY:
--     RJW 08/25/87  CREATED ORIGINAL TEST.
--     DHH 03/30/89  CHANGED MOD 4 TO A MACRO VALUE AND CHANGED
--                   EXTENSION FROM '.DEP' TO '.TST'.

-- MACRO SUBSTITUTION:
--     $ALIGNMENT IS THE VALUE USED TO ALIGN A RECORD ON A BOUNDARY
--     DEFINED BY THE IMPLEMENTATION.

WITH REPORT; USE REPORT;
WITH SYSTEM;
PROCEDURE CD4041A IS

     UNITS_PER_INTEGER : CONSTANT :=
                      (INTEGER'SIZE + SYSTEM.STORAGE_UNIT - 1) /
                      SYSTEM.STORAGE_UNIT;

     TYPE CHECK_CLAUSE IS RECORD
          INT_COMP    : INTEGER;
          CHAR_COMP   : CHARACTER;
     END RECORD;

     FOR CHECK_CLAUSE USE
          RECORD AT MOD $ALIGNMENT;
               INT_COMP AT 0
                           RANGE 0..INTEGER'SIZE - 1;
               CHAR_COMP AT 1*UNITS_PER_INTEGER
                           RANGE 0..CHARACTER'SIZE - 1;
          END RECORD;

     CHECK_RECORD : CHECK_CLAUSE := (1, 'A');

BEGIN
     TEST ("CD4041A", "CHECK THAT AN ALIGNMENT CLAUSE CAN BE " &
                      "GIVEN FOR A RECORD REPRESENTATION CLAUSE");

     IF CHECK_RECORD.INT_COMP'FIRST_BIT /= 0 THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF INT_COMP");
     END IF;

     IF CHECK_RECORD.INT_COMP'LAST_BIT /= INTEGER'SIZE - 1 THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF INT_COMP");
     END IF;

     IF CHECK_RECORD.INT_COMP'POSITION /= 0 THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF INT_COMP");
     END IF;

     IF CHECK_RECORD.CHAR_COMP'FIRST_BIT /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF CHAR_COMP");
     END IF;

     IF CHECK_RECORD.CHAR_COMP'LAST_BIT /=
               IDENT_INT (CHARACTER'SIZE - 1) THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF CHAR_COMP");
     END IF;

     IF CHECK_RECORD.CHAR_COMP'POSITION /=
               IDENT_INT (UNITS_PER_INTEGER) THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF CHAR_COMP");
     END IF;

     RESULT;
END CD4041A;
