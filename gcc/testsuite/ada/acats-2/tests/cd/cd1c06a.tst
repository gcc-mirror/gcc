-- CD1C06A.TST

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
--     CHECK THAT THE EXPRESSION IN A TASK STORAGE SIZE CLAUSE
--     IS NOT EVALUATED AGAIN WHEN A DERIVED TYPE INHERITS THE
--     STORAGE SIZE OF THE PARENT.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     JET 09/21/87  CREATED ORIGINAL TEST.
--     DHH 03/30/89  CHANGED SPECIFIED_SIZE TO A MACRO VALUE AND CHANGED
--                   EXTENSION FROM '.DEP' TO '.TST'.

WITH REPORT; USE REPORT;
PROCEDURE CD1C06A IS

     I : INTEGER := 0;

     SPECIFIED_SIZE : CONSTANT := $TASK_STORAGE_SIZE;

     FUNCTION COUNT_SIZE RETURN INTEGER IS
     BEGIN
          I := I + 1;
          RETURN SPECIFIED_SIZE * I;
     END;

BEGIN

     TEST("CD1C06A", "CHECK THAT THE EXPRESSION IN A TASK STORAGE " &
                     "SIZE CLAUSE IS NOT EVALUATED AGAIN WHEN A " &
                     "DERIVED TYPE INHERITS THE STORAGE SIZE OF " &
                     "THE PARENT");

     DECLARE

          TASK TYPE PARENT IS
               ENTRY E;
          END PARENT;

          FOR PARENT'STORAGE_SIZE USE COUNT_SIZE;

          TYPE DERIVED_TYPE IS NEW PARENT;

          TASK BODY PARENT IS
          BEGIN
               ACCEPT E DO
                    COMMENT ("ENTRY E ACCEPTED");
               END E;
          END PARENT;

     BEGIN
          IF PARENT'STORAGE_SIZE < IDENT_INT (SPECIFIED_SIZE) THEN
               FAILED ("PARENT'STORAGE_SIZE SHOULD NOT BE " &
                       "LESS THAN" & INTEGER'IMAGE (SPECIFIED_SIZE) &
                       ".  ACTUAL SIZE IS" &
                       INTEGER'IMAGE(PARENT'STORAGE_SIZE));
          END IF;

          IF DERIVED_TYPE'STORAGE_SIZE < IDENT_INT (SPECIFIED_SIZE) THEN
               FAILED ("DERIVED_TYPE'STORAGE_SIZE SHOULD NOT BE " &
                       "LESS THAN" & INTEGER'IMAGE(SPECIFIED_SIZE) &
                       ".  ACTUAL SIZE IS" &
                       INTEGER'IMAGE(DERIVED_TYPE'STORAGE_SIZE));
          END IF;

          IF I > IDENT_INT (1) THEN
               FAILED ("THE EXPRESSION FOR THE STORAGE SIZE " &
                       "SPECIFICATION WAS EVALUATED MORE THAN ONCE");
          END IF;

     END;

     RESULT;

END CD1C06A;
