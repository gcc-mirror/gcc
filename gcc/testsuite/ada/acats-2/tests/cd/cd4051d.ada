-- CD4051D.ADA

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
--     A DERIVED SUBTYPE WHOSE PARENT TYPE IS A RECORD TYPE WITH
--     VARIANTS AND THE REPRESENTATION CLAUSE MENTIONS COMPONENTS THAT
--     DO NOT EXIST IN THE DERIVED SUBTYPE.

-- HISTORY:
--     RJW 08/25/87  CREATED ORIGINAL TEST.
--     DHH 03/27/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND
--                   ADDED CHECK FOR REPRESENTATION CLAUSE.
--     RJW 10/26/89  REMOVED REFERENCES TO LENGTH_CHECK.
--     THS 09/18/90  MADE CALLS TO IDENT_INT TO DEFEAT OPTIMIZATION.
--     JRL 10/13/96  Adjusted ranges in type definitions to allow 1's
--                   complement machines to represent all values in
--                   the specified number of bits.

WITH REPORT; USE REPORT;
WITH SYSTEM;
PROCEDURE CD4051D IS

     TYPE INT IS RANGE -3 .. 3;
     TYPE LARGE_INT IS RANGE -7 .. 7;

     TYPE BASIC_CLAUSE (DISC : BOOLEAN) IS RECORD
          BOOL_COMP   : BOOLEAN;
          CASE DISC IS
               WHEN FALSE =>
                    INT_COMP  : LARGE_INT;
               WHEN TRUE  =>
                    CH_COMP_1 : INT;
                    CH_COMP_2 : INT;
          END CASE;
     END RECORD;

     TYPE CHECK_CLAUSE IS NEW BASIC_CLAUSE (TRUE);

     FOR CHECK_CLAUSE USE
          RECORD
               DISC AT 0
                           RANGE 0 .. 0;
               BOOL_COMP AT 0
                           RANGE 1 .. 1;
               INT_COMP AT 0
                           RANGE 2 .. 5;
               CH_COMP_1 AT 0
                           RANGE 2 .. 4;
               CH_COMP_2 AT 0
                           RANGE 5 .. 7;
          END RECORD;

     CHECK_RECORD : CHECK_CLAUSE := (TRUE, TRUE, -2, -2);

BEGIN
     TEST ("CD4051D", "CHECK THAT A RECORD REPRESENTATION " &
                      "CLAUSE CAN BE GIVEN FOR A DERIVED TYPE " &
                      "WHOSE PARENT TYPE IS A RECORD TYPE " &
                      "WITH VARIANTS AND WHERE THE RECORD " &
                      "REPRESENTATION CLAUSE MENTIONS COMPONENTS " &
                      "THAT DO NOT EXIST IN THE DERIVED SUBTYPE");

     IF CHECK_RECORD.DISC'FIRST_BIT /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF DISC");
     END IF;

     IF CHECK_RECORD.DISC'LAST_BIT /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF DISC");
     END IF;

     IF CHECK_RECORD.DISC'POSITION /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF DISC");
     END IF;

     IF CHECK_RECORD.BOOL_COMP'FIRST_BIT /= IDENT_INT (1) THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF BOOL_COMP");
     END IF;

     IF CHECK_RECORD.BOOL_COMP'LAST_BIT /= IDENT_INT (1) THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF BOOL_COMP");
     END IF;

     IF CHECK_RECORD.BOOL_COMP'POSITION /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF BOOL_COMP");
     END IF;

     IF CHECK_RECORD.CH_COMP_1'FIRST_BIT /= IDENT_INT (2) THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF CH_COMP_1");
     END IF;

     IF CHECK_RECORD.CH_COMP_1'LAST_BIT /= IDENT_INT (4) THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF CH_COMP_1");
     END IF;

     IF CHECK_RECORD.CH_COMP_1'POSITION /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF CH_COMP_1");
     END IF;

     IF CHECK_RECORD.CH_COMP_2'FIRST_BIT /= IDENT_INT (5) THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF CH_COMP_2");
     END IF;

     IF CHECK_RECORD.CH_COMP_2'LAST_BIT /= IDENT_INT (7) THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF CH_COMP_2");
     END IF;

     IF CHECK_RECORD.CH_COMP_2'POSITION /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF CH_COMP_2");
     END IF;

     RESULT;
END CD4051D;
