-- C93008B.ADA

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
-- CHECK THAT AFTER CREATION OF A TASK OBJECT BY AN ALLOCATOR, ANY
-- OPERATION INVOLVING THE RESULT DELIVERED BY THE ALLOCATOR IS
-- EXECUTED ONLY AFTER THE ACTIVATION OF THE TASK HAS COMPLETED.

-- WEI  3/ 4/82
-- TBN 12/20/85     RENAMED FROM C930AJA-B.ADA. ADDED DELAY STATEMENT
--                  DURING TASK ACTIVATION.
-- RJW 4/11/86      ADDED PACKAGE DUMMY.

with Impdef;
WITH REPORT; USE REPORT;
PROCEDURE C93008B IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     FUNCTION FINIT_POS (DIGT: IN ARG) RETURN NATURAL IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
          RETURN DIGT;
     END FINIT_POS;

BEGIN

     TEST ("C93008B", "USE OF RESULT AFTER CREATION OF " &
                      "A TASK BY ALLOCATOR");

BLOCK:
     DECLARE

          TASK TYPE TT1;

          TYPE ATT1 IS ACCESS TT1;
          TYPE ARRAY_ATT1 IS ARRAY (NATURAL RANGE 2 .. 3) OF ATT1;
          MY_ARRAY : ARRAY_ATT1;
          POINTER_TT1 : ATT1;

          TASK BODY TT1 IS
               PACKAGE DUMMY IS
               END DUMMY;

               PACKAGE BODY DUMMY IS
               BEGIN
                    DELAY 2.0 * Impdef.One_Second;
                    DECLARE
                         IDUMMY1 : NATURAL := FINIT_POS (1);
                    BEGIN
                         NULL;
                    END;
               END DUMMY;
          BEGIN
               NULL;
          END TT1;

     BEGIN

          MY_ARRAY := (2 => NEW TT1, 3 => NULL);  -- TASK ACTIVATED NOW.
          POINTER_TT1 := MY_ARRAY (FINIT_POS (2));

          MY_ARRAY (FINIT_POS (3)) := POINTER_TT1;

          IF SPYNUMB /= 123 THEN
               IF SPYNUMB = 132 OR SPYNUMB = 13 OR
                  SPYNUMB = 12  OR SPYNUMB = 1  OR
                  SPYNUMB = 0
               THEN
                    FAILED ("TASK ACTIVATION RIGHT IN TIME, " &
                            "BUT OTHER ERROR");
               ELSE
                    FAILED ("RESULT OF ALLOCATOR ACCESSED BEFORE " &
                            "TASK ACTIVATION HAS COMPLETED");
               END IF;
               COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
          END IF;
     END BLOCK;

     RESULT;

END C93008B;
