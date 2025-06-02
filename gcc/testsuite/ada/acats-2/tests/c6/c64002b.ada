-- C64002B.ADA

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
-- CHECK THAT PARAMETERLESS SUBPROGRAMS CAN BE CALLED WITH APPROPRIATE
--   NOTATION.

-- DAS  1/27/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C64002B IS

     USE REPORT;

     I    : INTEGER := 1;

     FUNCTION F0 RETURN INTEGER IS
     BEGIN
          RETURN 7;
     END F0;

     PROCEDURE P0 IS
     BEGIN
          I := 15;
     END P0;

BEGIN

     TEST ("C64002B", "CHECK THAT PARAMETERLESS SUBPROGRAMS CAN BE" &
                      " CALLED");

     IF (F0 /= 7) THEN
          FAILED ("PARAMETERLESS FUNCTION CALL RETURNS BAD VALUE");
     END IF;

     P0;
     IF (I /= 15) THEN
          FAILED ("PARAMETERLESS PROCEDURE CALL YIELDS INCORRECT" &
                  " RESULT");
     END IF;

     RESULT;

END C64002B;
