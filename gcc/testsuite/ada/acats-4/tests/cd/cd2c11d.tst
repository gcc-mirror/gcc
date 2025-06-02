--CD2C11D.TST

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
--     CHECK THAT THE EXPRESSION IN A TASK STORAGE SIZE CLAUSE NEED
--     NOT BE STATIC.

-- MACRO SUBSTITUTION:
--     $TASK_STORAGE_SIZE IS THE NUMBER OF STORAGE_UNITS REQUIRED FOR
--     THE ACTIVATION OF A TASK.

-- HISTORY
--     DHH 09/29/87 CREATED ORIGINAL TEST
--     PWB 05/11/89 CHANGED EXTENSION FROM '.DEP' TO '.TST'.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE CD2C11D IS

BEGIN

     TEST ("CD2C11D","THE EXPRESSION IN A TASK STORAGE SIZE CLAUSE " &
                     "NEED NOT BE STATIC");

     DECLARE

          STORAGE_SIZE : CONSTANT := $TASK_STORAGE_SIZE;
          PACKAGE PACK IS
               TASK TYPE CHECK_TYPE;

               FOR CHECK_TYPE'STORAGE_SIZE USE
                                      STORAGE_SIZE;
               TASK TYPE TTYPE IS
                    ENTRY ADD(J :IN INTEGER; K : IN OUT INTEGER);
               END TTYPE;

               FOR TTYPE'STORAGE_SIZE USE IDENT_INT(STORAGE_SIZE);

          END PACK;

          PACKAGE BODY PACK IS

               TASK BODY TTYPE IS
               BEGIN
                    ACCEPT ADD(J :IN INTEGER; K : IN OUT INTEGER);
               END TTYPE;

               TASK BODY CHECK_TYPE IS
               BEGIN
                    NULL;
               END CHECK_TYPE;

          BEGIN

               IF TTYPE'STORAGE_SIZE < IDENT_INT(STORAGE_SIZE) THEN
                    FAILED("STORAGE_SIZE SPECIFIED IS " &
                           "GREATER THAN MEMORY ALLOCATED");
               END IF;

          END PACK;
     BEGIN
          NULL;
     END;

     RESULT;
END CD2C11D;
