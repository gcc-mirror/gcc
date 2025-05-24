-- CD7204C.ADA

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
--     CHECK THAT THE PREFIX OF THE 'POSITION, 'LAST_BIT, AND 'FIRST_BIT
--     ATTRIBUTES CAN DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES
--     RETURN APPROPRIATE VALUES WHEN A RECORD REPRESENTATION CLAUSE
--     IS GIVEN.

-- HISTORY:
--     BCB 09/14/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH SYSTEM;
WITH REPORT;  USE REPORT;

PROCEDURE CD7204C IS

     UNITS_PER_INTEGER : CONSTANT :=
          (INTEGER'SIZE + SYSTEM.STORAGE_UNIT - 1)/SYSTEM.STORAGE_UNIT;

     TYPE BASIC_REC IS RECORD
          CHECK_INT : INTEGER;
          CHECK_CHAR : CHARACTER;
     END RECORD;

     FOR BASIC_REC USE
          RECORD
               CHECK_INT AT 0 RANGE 0..INTEGER'SIZE - 1;
               CHECK_CHAR AT 1*UNITS_PER_INTEGER
                          RANGE 0..CHARACTER'SIZE - 1;
          END RECORD;

     CHECK_REC : BASIC_REC;

BEGIN

     TEST ("CD7204C", "THE PREFIX OF THE 'POSITION, " &
                      "'LAST_BIT, AND 'FIRST_BIT ATTRIBUTES CAN " &
                      "DENOTE A RECORD COMPONENT, AND THE ATTRIBUTES " &
                      "RETURN APPROPRIATE VALUES WHEN A RECORD " &
                      "REPRESENTATION CLAUSE IS GIVEN");

     IF CHECK_REC.CHECK_INT'POSITION /= 0 THEN
          FAILED ("INCORRECT VALUE FOR POSITION OF CHECK_INT");
     END IF;

     IF CHECK_REC.CHECK_INT'FIRST_BIT /= IDENT_INT (0) THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF CHECK_INT");
     END IF;

     IF CHECK_REC.CHECK_INT'LAST_BIT /= INTEGER'SIZE - 1 THEN
          FAILED ("INCORRECT VALUE FOR LAST_BIT OF CHECK_INT");
     END IF;

     IF CHECK_REC.CHECK_CHAR'POSITION /= IDENT_INT (UNITS_PER_INTEGER)
          THEN FAILED ("INCORRECT VALUE FOR POSITION OF CHECK_CHAR");
     END IF;

     IF CHECK_REC.CHECK_CHAR'FIRST_BIT /= 0 THEN
          FAILED ("INCORRECT VALUE FOR FIRST_BIT OF CHECK_CHAR");
     END IF;

     IF CHECK_REC.CHECK_CHAR'LAST_BIT /= IDENT_INT (CHARACTER'SIZE - 1)
          THEN FAILED ("INCORRECT VALUE FOR LAST_BIT OF CHECK_CHAR");
     END IF;

     RESULT;

END CD7204C;
