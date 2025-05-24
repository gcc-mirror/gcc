-- CD4031A.ADA

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
--     CHECK THAT WHEN A RECORD REPRESENTATION CLAUSE IS GIVEN FOR A
--     VARIANT RECORD TYPE, THEN COMPONENTS BELONGING TO DIFFERENT
--     VARIANTS CAN BE GIVEN OVERLAPPING STORAGE.

-- HISTORY:
--     PWB 07/22/87  CREATED ORIGINAL TEST.
--     DHH 03/27/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA' AND
--                   ADDED CHECK FOR REPRESENTATION CLAUSE.
--     RJW 06/12/90  REMOVED REFERENCES TO LENGTH_CHECK.  REVISED
--                   COMMENTS.
--     JRL 10/13/96  Adjusted ranges in type definitions to allow 1's
--                   complement machines to represent all values in
--                   the specified number of bits.

WITH REPORT; USE REPORT;
PROCEDURE CD4031A IS

     TYPE DISCRIMINAN IS RANGE -1 .. 1;
     TYPE INT IS RANGE -3 .. 3;
     TYPE LARGE_INT IS RANGE -7 .. 7;

     TYPE TEST_CLAUSE (DISC : DISCRIMINAN := 0) IS
          RECORD
               CASE DISC IS
                    WHEN 0 =>
                         INTEGER_COMP : LARGE_INT;
                    WHEN OTHERS  =>
                         CH_COMP_1 : INT;
                         CH_COMP_2 : INT;
               END CASE;
          END RECORD;

     FOR TEST_CLAUSE USE
          RECORD
               DISC AT 0
                           RANGE 0 .. 1;
               INTEGER_COMP AT 0
                           RANGE 2 .. 5;
               CH_COMP_1 AT 0
                           RANGE 2 .. 4;
               CH_COMP_2 AT 0
                           RANGE 5 .. 7;
          END RECORD;

     TYPE TEST_CL1 IS NEW TEST_CLAUSE(DISC => 0);
     TYPE TEST_CL2 IS NEW TEST_CLAUSE(DISC => 1);
     TEST_RECORD : TEST_CL1;
     TEST_RECORD1 : TEST_CL2;

     INTEGER_COMP_FIRST,
     CH_COMP_1_FIRST : INTEGER;

BEGIN
     TEST ("CD4031A", "IN RECORD REPRESENTATION CLAUSES " &
                      "FOR VARIANT RECORD TYPES, " &
                      "COMPONENTS OF DIFFERENT VARIANTS " &
                      "CAN BE GIVEN OVERLAPPING STORAGE");

     TEST_RECORD := (0, -7);
     INTEGER_COMP_FIRST := TEST_RECORD.INTEGER_COMP'FIRST_BIT;

     TEST_RECORD1 := (1, -3, -3);
     CH_COMP_1_FIRST := TEST_RECORD1.CH_COMP_1'FIRST_BIT;

     IF INTEGER_COMP_FIRST /= CH_COMP_1_FIRST THEN
          FAILED ("COMPONENTS DO NOT BEGIN AT SAME POINT");
     END IF;

     RESULT;
END CD4031A;
