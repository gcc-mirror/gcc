-- C38102C.ADA

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
--     CHECK THAT INCOMPLETE TYPES CAN BE FIXED.

-- HISTORY:
--     DAT 03/24/81  CREATED ORIGINAL TEST.
--     SPS 10/25/82
--     BCB 10/21/87  CHANGED HEADER TO STANDARD FORMAT.  CHANGED VARIOUS
--                   VALUES TO CORRECT CONSTRAINT PROBLEMS.  CHANGED
--                   THE VALUE OF F'DELTA, USING A POWER OF TWO.

WITH REPORT; USE REPORT;

PROCEDURE C38102C IS
BEGIN
     TEST ("C38102C", "INCOMPLETE TYPE CAN BE FIXED");

     DECLARE

          TYPE F;
          TYPE G;
          TYPE AF IS ACCESS F;
          TYPE F IS DELTA 0.25 RANGE -2.0 .. 2.0;
          TYPE G IS NEW F RANGE -1.0 .. 1.5;
          TYPE AG IS ACCESS G RANGE -0.75 .. 1.25;

          XF : AF := NEW F '(1.0);
          XG : AG := NEW G '(G (XF.ALL/2));

     BEGIN
          IF XG.ALL NOT IN G THEN
               FAILED ("ACCESS TO FIXED");
          END IF;
     END;

     RESULT;
END C38102C;
