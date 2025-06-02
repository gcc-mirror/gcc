-- C38102D.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE CAN BE REDECLARED AS A TASK TYPE.

-- AH    8/14/86

WITH REPORT; USE REPORT;
PROCEDURE C38102D IS
     GLOBAL : INTEGER := 0;
BEGIN
     TEST("C38102D", "INCOMPLETE TYPES CAN BE TASKS");
     DECLARE
          TYPE T1;
          TASK TYPE T1 IS
               ENTRY E(LOCAL : IN OUT INTEGER);
          END T1;
          T1_OBJ : T1;
          TASK BODY T1 IS
          BEGIN
               ACCEPT E(LOCAL : IN OUT INTEGER) DO
                    LOCAL := IDENT_INT(2);
               END E;
          END T1;
     BEGIN
          T1_OBJ.E(GLOBAL);
     END;

     IF GLOBAL /= IDENT_INT(2) THEN
          FAILED ("TASK NOT EXECUTED");
     END IF;
     RESULT;
END C38102D;
