-- CD2B11B.ADA

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
--     CHECK THAT IF A COLLECTION SIZE IS SPECIFIED FOR AN
--     ACCESS TYPE IN A GENERIC UNIT, THEN OPERATIONS ON VALUES OF THE
--     ACCESS TYPE ARE NOT AFFECTED.

-- HISTORY:
--     BCB 09/23/87  CREATED ORIGINAL TEST.
--     PWB 05/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA'.

WITH REPORT;  USE REPORT;

PROCEDURE CD2B11B IS

     BASIC_SIZE : CONSTANT := 1024;
     B : BOOLEAN;

BEGIN

     TEST ("CD2B11B", "CHECK THAT IF A COLLECTION SIZE IS SPECIFIED " &
                      "FOR AN ACCESS TYPE, THEN " &
                      "OPERATIONS ON VALUES OF THE ACCESS TYPE ARE " &
                      "NOT AFFECTED");

     DECLARE

          GENERIC
          FUNCTION FUNC RETURN BOOLEAN;

          FUNCTION FUNC RETURN BOOLEAN IS

               TYPE MAINTYPE IS ARRAY (INTEGER RANGE <>) OF INTEGER;
               TYPE ACC_TYPE IS ACCESS MAINTYPE;
               SUBTYPE ACC_RANGE IS ACC_TYPE (1 .. 3);

               FOR ACC_TYPE'STORAGE_SIZE
                    USE BASIC_SIZE;

               TYPE RECORD_TYPE IS RECORD
                    COMP : ACC_TYPE;
               END RECORD;

               CHECK_TYPE1 : ACC_TYPE;
               CHECK_TYPE2 : ACC_TYPE;
               CHECK_TYPE3 : ACC_TYPE(1..3);

               CHECK_ARRAY : ARRAY (1..3) OF ACC_TYPE;

               CHECK_RECORD1 : RECORD_TYPE;
               CHECK_RECORD2 : RECORD_TYPE;

               CHECK_PARAM1 : ACC_TYPE;
               CHECK_PARAM2 : ACC_TYPE;

               CHECK_NULL : ACC_TYPE := NULL;

               PROCEDURE PROC (ACC1,ACC2 : IN OUT ACC_TYPE) IS

               BEGIN

                    IF (ACC1.ALL /= ACC2.ALL) THEN
                         FAILED ("INCORRECT VALUES FOR DESIGNATED " &
                                 "OBJECTS - 1");
                    END IF;

                    IF EQUAL (3,3) THEN
                         ACC2 := ACC1;
                    END IF;

                    IF ACC2 /= ACC1 THEN
                         FAILED ("INCORRECT RESULTS FOR RELATIONAL " &
                                 "OPERATORS - 1");
                    END IF;

                    IF (ACC1 IN ACC_RANGE) THEN
                         FAILED ("INCORRECT RESULTS FOR " &
                                 "MEMBERSHIP TEST - 1");
                    END IF;

               END PROC;

     BEGIN -- FUNC.

          CHECK_PARAM1 := NEW MAINTYPE'(25,35,45);
          CHECK_PARAM2 := NEW MAINTYPE'(25,35,45);

          PROC (CHECK_PARAM1,CHECK_PARAM2);

          IF ACC_TYPE'STORAGE_SIZE < BASIC_SIZE THEN
               FAILED ("INCORRECT VALUE FOR ACCESS TYPE STORAGE_SIZE");
          END IF;

          CHECK_TYPE1 := NEW MAINTYPE'(25,35,45);
          CHECK_TYPE2 := NEW MAINTYPE'(25,35,45);
          CHECK_TYPE3 := NEW MAINTYPE'(1 => 1,2 => 2,3 => 3);

          CHECK_ARRAY (1) := NEW MAINTYPE'(25,35,45);
          CHECK_ARRAY (2) := NEW MAINTYPE'(25,35,45);

          CHECK_RECORD1.COMP := NEW MAINTYPE'(25,35,45);
          CHECK_RECORD2.COMP := NEW MAINTYPE'(25,35,45);

          IF (CHECK_TYPE1.ALL /= CHECK_TYPE2.ALL) THEN
               FAILED ("INCORRECT VALUES FOR DESIGNATED OBJECTS - 2");
          END IF;

          IF EQUAL (3,3) THEN
               CHECK_TYPE2 := CHECK_TYPE1;
          END IF;

          IF CHECK_TYPE2 /= CHECK_TYPE1 THEN
               FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " &
                       "- 2");
          END IF;

          IF (CHECK_TYPE1 IN ACC_RANGE) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP TEST - 2");
          END IF;

          IF (CHECK_ARRAY (1).ALL /= CHECK_ARRAY (2).ALL) THEN
               FAILED ("INCORRECT VALUES FOR DESIGNATED OBJECTS - 3");
          END IF;

          IF EQUAL (3,3) THEN
               CHECK_ARRAY (2) := CHECK_ARRAY (1);
          END IF;

          IF CHECK_ARRAY (2) /= CHECK_ARRAY (1) THEN
               FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " &
                       "- 3");
          END IF;

          IF (CHECK_ARRAY (1) IN ACC_RANGE) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP TEST - 3");
          END IF;

          IF (CHECK_RECORD1.COMP.ALL /= CHECK_RECORD2.COMP.ALL) THEN
               FAILED ("INCORRECT VALUES FOR DESIGNATED OBJECTS - 4");
          END IF;

          IF EQUAL (3,3) THEN
               CHECK_RECORD2 := CHECK_RECORD1;
          END IF;

          IF CHECK_RECORD2 /= CHECK_RECORD1 THEN
               FAILED ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " &
                       "- 4");
          END IF;

          IF (CHECK_RECORD1.COMP IN ACC_RANGE) THEN
               FAILED ("INCORRECT RESULTS FOR MEMBERSHIP TEST - 4");
          END IF;

          IF CHECK_TYPE3'FIRST /= IDENT_INT (1) THEN
               FAILED ("INCORRECT VALUE FOR CHECK_TYPE3'FIRST");
          END IF;

          IF CHECK_TYPE3'LAST /= IDENT_INT (3) THEN
               FAILED ("INCORRECT VALUE FOR CHECK_TYPE3'LAST");
          END IF;

          RETURN TRUE;

     END FUNC;

     FUNCTION NEWFUNC IS NEW FUNC;

     BEGIN
          B := NEWFUNC;
     END;

     RESULT;
END CD2B11B;
