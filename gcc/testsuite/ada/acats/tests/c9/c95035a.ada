-- C95035A.ADA

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
-- CHECK THAT A TASK IS SUSPENDED IF IT REACHES AN ACCEPT STATEMENT
-- PRIOR TO ANY CALL OF THE CORRESPONDING ENTRY.

-- WEI  3/ 4/82
-- JWC 6/28/85   RENAMED FROM C950CAA-B.ADA

with Impdef;
WITH REPORT;
 USE REPORT;
PROCEDURE C95035A IS

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     PROCEDURE PSPY_NUMB (DIGT: IN ARG) IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
     END PSPY_NUMB;

     TASK T1 IS
          ENTRY E1;
          ENTRY BYE;
     END T1;

     TASK BODY T1 IS
     BEGIN
          ACCEPT E1;
          PSPY_NUMB (2);
          ACCEPT BYE;
     END T1;

     TASK T2;

     TASK BODY T2 IS
     BEGIN
          DELAY 1.0 * Impdef.One_Second;
          PSPY_NUMB (1);
          T1.E1;
     END T2;

BEGIN

     TEST ("C95035A", "TASK SUSPENSION PRIOR TO ENTRY CALL");

     T1.BYE;

     IF SPYNUMB /= 12 THEN
          FAILED ("ERROR DURING TASK EXECUTION");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     RESULT;

END C95035A;
