-- C95034B.ADA

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
-- CHECK THAT A CALLING TASK REMAINS SUSPENDED UNTIL THE ACCEPT
-- STATEMENT RECEIVING THIS ENTRY CALL HAS COMPLETED THE EXECUTION OF
-- ITS SEQUENCE OF STATEMENTS.

-- WEI  3/ 4/82
-- JWC 6/28/85   RENAMED FROM C950CBA-B.ADA

with Impdef;
WITH REPORT;
 USE REPORT;
PROCEDURE C95034B IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     PROCEDURE PSPY_NUMB (DIGT: IN ARG) IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
     END PSPY_NUMB;

     TASK T1 IS
          ENTRY E1;
     END T1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT E1 DO
               PSPY_NUMB (1);
               DELAY 1.0 * Impdef.One_Second;
               PSPY_NUMB (2);
          END E1;
     END T1;

     TASK T2 IS
          ENTRY BYE;
     END T2;

     TASK BODY T2 IS
     BEGIN
          T1.E1;
          PSPY_NUMB (3);
          ACCEPT BYE;
     END T2;

BEGIN

     TEST ("C95034B", "TASK SUSPENSION UNTIL COMPLETION OF ACCEPT " &
                      "STATEMENT");

     T2.BYE;

     IF SPYNUMB /= 123 THEN
          FAILED ("ERROR DURING TASK EXECUTION");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     RESULT;

END C95034B;
