-- CE3708A.ADA

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
--     CHECK THAT INTEGER_IO PUT RAISES LAYOUT_ERROR WHEN THE MINIMUM
--     WIDTH REQUIRED FOR THE OUTPUT VALUE IS GREATER THAN THE LENGTH
--     OF THE STRING.  ALSO CHECK THAT INTEGER_IO PUT PADS THE OUTPUT
--     ON THE LEFT WITH SPACES IF THE LENGTH OF THE STRING IS GREATER
--     THAN THE MINIMUM WIDTH REQUIRED.

-- HISTORY:
--     SPS 10/05/82
--     CPP 07/30/84
--     JLH 09/11/87  ADDED CASES FOR PADDING OF OUTPUT STRING.

WITH REPORT; USE REPORT;
WITH TEXT_IO; USE TEXT_IO;

PROCEDURE CE3708A IS
BEGIN

     TEST ("CE3708A", "CHECK THAT INTEGER_IO PUT RAISES LAYOUT_ERROR " &
                      "WHEN THE MINIMUM WIDTH REQUIRED FOR THE " &
                      "OUTPUT VALUE IS GREATER THAN THE LENGTH OF " &
                      "THE STRING.  ALSO CHECK THAT INTEGER_IO PUT " &
                      "PADS THE OUTPUT ON THE LEFT WITH SPACES IF " &
                      "THE LENGTH OF THE STRING IS GREATER THAN THE " &
                      "MINIMUM WIDTH REQUIRED.");

     DECLARE
          PACKAGE IIO IS NEW INTEGER_IO (INTEGER);
          USE IIO;
          ST1 : STRING (1 .. 4);
          ST2 : STRING (1 .. 4);
          ST : STRING (1 .. 4) := "6382";
     BEGIN
          PUT (ST1, IDENT_INT(6382));
          IF ST1 /= ST THEN
               FAILED ("PUT TO STRING INCORRECT");
          END IF;

          BEGIN
               PUT (ST2, IDENT_INT(12345));
               FAILED ("LAYOUT_ERROR NOT RAISED");
          EXCEPTION
               WHEN LAYOUT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED");
          END;

          PUT (ST1, IDENT_INT(123));
          IF ST1 /= " 123" THEN
               FAILED ("PUT DID NOT PAD WITH BLANKS - 1");
          END IF;

          PUT (ST2, IDENT_INT(-2));
          IF ST2 /= "  -2" THEN
               FAILED ("PUT DID NOT PAD WITH BLANKS - 2");
          END IF;

     END;

     RESULT;

END CE3708A;
