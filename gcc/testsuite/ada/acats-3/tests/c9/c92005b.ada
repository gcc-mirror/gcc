-- C92005B.ADA

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
-- CHECK THAT FOR A TASK OBJECT CREATED BY AN ALLOCATOR THE
-- OBJECT VALUE IS SET DURING EXECUTION OF THE ALLOCATOR.

-- WEI  3/ 4/82
-- JBG  5/25/85
-- RLB  1/ 7/05

WITH REPORT;
 USE REPORT;
WITH SYSTEM;
PROCEDURE C92005B IS
     TYPE BIG_INT IS RANGE 0..SYSTEM.MAX_INT;
BEGIN
     TEST ("C92005B", "TASK VALUE SET BY EXECUTION OF ALLOCATOR");

BLOCK:
     DECLARE
          TASK TYPE TT1;

          TYPE ATT1 IS ACCESS TT1;

          TASK BODY TT1 IS
          BEGIN
               NULL;
          END TT1;

          PACKAGE PACK IS
          END PACK;

          PACKAGE BODY PACK IS
               POINTER_TT1 : ATT1 := NEW TT1;
               I : BIG_INT := POINTER_TT1.ALL'STORAGE_SIZE;
          BEGIN
               IF NOT EQUAL(INTEGER(I MOD 1024), INTEGER(I MOD 1024)) THEN
                    FAILED ("UNEXPECTED PROBLEM");
               END IF;
          END PACK;
     BEGIN
          NULL;
     EXCEPTION
          WHEN PROGRAM_ERROR | CONSTRAINT_ERROR =>
               FAILED ("TASK OBJECT VALUE NOT SET DURING " &
                       "EXECUTION OF ALLOCATOR");
     END BLOCK;

     RESULT;

END C92005B;
