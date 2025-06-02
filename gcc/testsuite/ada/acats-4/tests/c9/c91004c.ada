-- C91004C.ADA

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
-- CHECK THAT A TASK (TYPE) IDENTIFIER, WHEN USED WITHIN ITS OWN BODY
-- REFERS TO THE EXECUTING TASK.
--
-- TEST USING CONDITIONAL ENTRY CALL.

-- WEI  3/ 4/82
-- TLB 10/30/87  RENAMED FROM C910BDB.ADA.

WITH REPORT;
 USE REPORT;
PROCEDURE C91004C IS

     TASK TYPE TT1 IS
          ENTRY E1;
          ENTRY BYE;
     END TT1;

     OBJ_TT1 : ARRAY (NATURAL RANGE 1..2) OF TT1;

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     PROCEDURE PSPY_NUMB (DIGT: IN ARG) IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
     END PSPY_NUMB;

     TASK BODY TT1 IS
     BEGIN
          ACCEPT E1 DO
               PSPY_NUMB (1);
          END E1;

          SELECT
               TT1.E1;
          ELSE
               PSPY_NUMB (2);
          END SELECT;

          ACCEPT BYE;
     END TT1;

BEGIN

     TEST ("C91004C", "TASK IDENTIFIER IN OWN BODY");
     OBJ_TT1 (1).E1;
     OBJ_TT1 (1).BYE;

     IF SPYNUMB /=12 THEN
          FAILED ("WRONG TASK OBJECT REFERENCED");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     ABORT OBJ_TT1 (2);

     RESULT;

END C91004C;
