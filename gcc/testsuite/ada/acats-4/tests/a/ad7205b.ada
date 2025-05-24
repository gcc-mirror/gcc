-- AD7205B.ADA

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
--     CHECK THAT THE PREFIX OF THE 'STORAGE_SIZE ATTRIBUTE CAN BE AN
--     ACCESS TYPE, A TASK TYPE, A TASK OBJECT, OR A SINGLE TASK.

-- HISTORY:
--     JET 09/22/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE AD7205B IS

     B : BOOLEAN;

     TYPE A IS ACCESS INTEGER;
     TASK TYPE T;
     T1 : T;
     TASK T2;

     TASK BODY T IS
     BEGIN
          NULL;
     END T;

     TASK BODY T2 IS
     BEGIN
          NULL;
     END T2;

BEGIN

     TEST ("AD7205B", "CHECK THAT THE PREFIX OF THE 'STORAGE_SIZE " &
                      "ATTRIBUTE CAN BE AN ACCESS TYPE, A TASK TYPE, " &
                      "A TASK OBJECT, OR A SINGLE TASK");

     B := A'STORAGE_SIZE = T'STORAGE_SIZE;   -- ACCESS AND TASK TYPES.
     B := T1'STORAGE_SIZE = T2'STORAGE_SIZE; -- TASK OBJECT & SINGLE
                                             --   TASK.

     RESULT;

END AD7205B;
