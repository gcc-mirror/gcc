-- CD2B11F.ADA

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
--     CHECK THAT IF A COLLECTION SIZE SPECIFICATION IS GIVEN FOR AN
--     ACCESS TYPE WHOSE DESIGNATED TYPE IS A DISCRIMINATED RECORD, THEN
--     OPERATIONS ON VALUES OF THE ACCESS TYPE ARE NOT AFFECTED.

-- HISTORY:
--     BCB 09/29/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT;  USE REPORT;

PROCEDURE CD2B11F IS

     BASIC_SIZE : CONSTANT := 1024;

     TYPE RECORD_TYPE(DISC : INTEGER := 100) IS RECORD
          COMP1 : INTEGER;
          COMP2 : INTEGER;
          COMP3 : INTEGER;
     END RECORD;

     TYPE ACC_RECORD IS ACCESS RECORD_TYPE;
     FOR ACC_RECORD'STORAGE_SIZE USE BASIC_SIZE;

     CHECK_RECORD1 : ACC_RECORD;
     CHECK_RECORD2 : ACC_RECORD;

BEGIN

     TEST ("CD2B11F", "CHECK THAT IF A COLLECTION SIZE SPECIFICATION " &
                      "IS GIVEN FOR AN ACCESS TYPE WHOSE " &
                      "DESIGNATED TYPE IS A DISCRIMINATED RECORD, " &
                      "THEN OPERATIONS ON VALUES OF THE ACCESS TYPE " &
                      "ARE NOT AFFECTED");

     CHECK_RECORD1 := NEW RECORD_TYPE;
     CHECK_RECORD1.COMP1 := 25;
     CHECK_RECORD1.COMP2 := 25;
     CHECK_RECORD1.COMP3 := 150;

     IF ACC_RECORD'STORAGE_SIZE < BASIC_SIZE THEN
          FAILED ("INCORRECT VALUE FOR RECORD TYPE ACCESS " &
                  "STORAGE_SIZE");
     END IF;

     IF CHECK_RECORD1.DISC /= IDENT_INT (100) THEN
          FAILED ("INCORRECT VALUE FOR RECORD DISCRIMINANT");
     END IF;

     IF ((CHECK_RECORD1.COMP1 /= CHECK_RECORD1.COMP2) OR
             (CHECK_RECORD1.COMP1 = CHECK_RECORD1.COMP3)) THEN
          FAILED ("INCORRECT VALUE FOR RECORD COMPONENT");
     END IF;

     IF EQUAL (3,3) THEN
          CHECK_RECORD2 := CHECK_RECORD1;
     END IF;

     IF CHECK_RECORD2 /= CHECK_RECORD1 THEN
          FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATOR");
     END IF;

     RESULT;
END CD2B11F;
