-- C92006A.ADA

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
-- CHECK THAT TASK OBJECTS CAN BE INTERCHANGED BY ASSIGNMENT OF
-- CORRESPONDING ACCESS TYPE OBJECTS.

-- WEI  3/ 4/82
-- JWC 6/28/85   RENAMED FROM C920BIA-B.ADA

WITH REPORT;
 USE REPORT;
PROCEDURE C92006A IS

     TASK TYPE TT1 IS
          ENTRY E1;
          ENTRY E2;
     END TT1;

     TYPE ATT1 IS ACCESS TT1;
     POINTER_TT1_1, POINTER_TT1_2 : ATT1;

     SUBTYPE ARG IS NATURAL RANGE 0..9;
     SPYNUMB : NATURAL := 0;

     PROCEDURE PSPY_NUMB (DIGT: IN ARG) IS
     BEGIN
          SPYNUMB := 10*SPYNUMB+DIGT;
     END PSPY_NUMB;

     PROCEDURE PROC (P1, P2 : IN OUT ATT1) IS
     -- SWAP TASK OBJECTS P1, P2.
          SCRATCH : ATT1;
     BEGIN
          SCRATCH := P1;
          P1 := P2;
          P2 := SCRATCH;

          P1.E2;  -- ENTRY2 SECOND OBJECT.
          P2.E1;  -- VICE VERSA.

     END PROC;

     TASK BODY TT1 IS
     BEGIN
          ACCEPT E1 DO
               PSPY_NUMB (1);
          END E1;
          ACCEPT E2 DO
               PSPY_NUMB (2);
          END E2;
     END TT1;

BEGIN

     TEST ("C92006A", "INTERCHANGING TASK OBJECTS");
     POINTER_TT1_1 := NEW TT1;
     POINTER_TT1_2 := NEW TT1;

     POINTER_TT1_2.ALL.E1;
     PROC (POINTER_TT1_1, POINTER_TT1_2);
     POINTER_TT1_2.E2;        -- E2 OF FIRST OBJECT
-- EACH ENTRY OF EACH TASK OBJECT SHOULD HAVE BEEN CALLED.

     IF SPYNUMB /= 1212 THEN
          FAILED ("FAILURE TO SWAP TASK OBJECTS " &
                  "IN PROCEDURE PROC");
          COMMENT ("ACTUAL ORDER WAS:" & INTEGER'IMAGE(SPYNUMB));
     END IF;

     RESULT;

END C92006A;
