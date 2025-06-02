-- C92005A.ADA

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
-- CHECK THAT FOR A NON-SINGLE TASK THE OBJECT VALUE IS SET DURING
-- ELABORATION OF THE CORRESPONDING OBJECT DECLARATION.

-- WEI  3/ 4/82
-- JBG 5/25/85
-- PWB  2/3/86    CORRECTED TEST ERROR; ADDED 'USE' CLAUSE TO MAKE "/="
--                FOR BIG_INT VISIBLE.

WITH REPORT, SYSTEM;
 USE REPORT;
PROCEDURE C92005A IS
BEGIN

     TEST ("C92005A", "TASK OBJECT VALUE DURING ELABORATION");

     DECLARE
          TASK TYPE TT1;

          OBJ_TT1 : TT1;

          PACKAGE PACK IS
               TYPE BIG_INT IS RANGE 0 .. SYSTEM.MAX_INT;
               I : BIG_INT;
          END PACK;

          PACKAGE BODY PACK IS
          BEGIN
               I := OBJ_TT1'STORAGE_SIZE;  -- O.K.
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("TASK OBJECT RAISED EXCEPTION");
          END PACK;

          USE PACK;

          TASK BODY TT1 IS
          BEGIN
               NULL;
          END TT1;

     BEGIN
          IF PACK.I /= OBJ_TT1'STORAGE_SIZE THEN
               COMMENT ("STORAGE SIZE CHANGED AFTER TASK ACTIVATED");
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTION RAISED BY STORAGE_SIZE");
     END;

     RESULT;
END C92005A;
