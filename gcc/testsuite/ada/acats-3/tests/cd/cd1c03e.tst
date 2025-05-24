-- CD1C03E.TST

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
--     CHECK THAT THE STORAGE SIZE OF A DERIVED TASK TYPE IS
--     INHERITED FROM THE PARENT IF THE STORAGE SIZE OF THE
--     PARENT WAS DETERMINED BY A TASK STORAGE SIZE CLAUSE.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY:
--     JET 09/16/87  CREATED ORIGINAL TEST.
--     DHH 03/30/89  CHANGED SPECIFIED_SIZE TO A MACRO VALUE AND CHANGED
--                   EXTENSION FROM '.DEP' TO '.TST'.

WITH REPORT; USE REPORT;
PROCEDURE CD1C03E IS

     SPECIFIED_SIZE : CONSTANT := $TASK_STORAGE_SIZE;

     TASK TYPE PARENT_TYPE IS
          ENTRY E;
     END PARENT_TYPE;

     FOR PARENT_TYPE'STORAGE_SIZE USE SPECIFIED_SIZE;

     TYPE DERIVED_TYPE IS NEW PARENT_TYPE;

     TASK BODY PARENT_TYPE IS
     BEGIN
          ACCEPT E DO
               COMMENT ("ENTRY E ACCEPTED");
          END E;
     END PARENT_TYPE;

BEGIN

     TEST("CD1C03E", "CHECK THAT THE STORAGE SIZE OF A DERIVED " &
                     "TASK TYPE IS INHERITED FROM THE PARENT IF " &
                     "THE STORAGE SIZE OF THE PARENT WAS " &
                     "DETERMINED BY A TASK STORAGE SIZE CLAUSE");

     IF PARENT_TYPE'STORAGE_SIZE < IDENT_INT (SPECIFIED_SIZE) THEN
          FAILED ("PARENT_TYPE'STORAGE_SIZE SHOULD NOT BE LESS THAN" &
                  INTEGER'IMAGE(SPECIFIED_SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(PARENT_TYPE'STORAGE_SIZE));
     END IF;

     IF DERIVED_TYPE'STORAGE_SIZE < IDENT_INT (SPECIFIED_SIZE) THEN
          FAILED ("DERIVED_TYPE'STORAGE_SIZE SHOULD NOT BE LESS THAN " &
                  INTEGER'IMAGE(SPECIFIED_SIZE) &
                  ".  ACTUAL SIZE IS" &
                  INTEGER'IMAGE(DERIVED_TYPE'STORAGE_SIZE));
     END IF;

     RESULT;

END CD1C03E;
