-- CE3002F.ADA

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
-- CHECK THAT UNBOUNDED HAS TYPE COUNT AND VALUE ZERO.

-- SPS 10/1/82
-- SPS 11/9/82

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3002F IS
BEGIN

     TEST ("CE3002F", "CHECK THAT UNBOUNDED HAS TYPE COUNT AND " &
                      "VALUE ZERO");

     DECLARE
          Z : COUNT := 0;
     BEGIN
          IF UNBOUNDED /= COUNT(IDENT_INT(0)) THEN
               FAILED ("UNBOUNDED NOT 0");
          END IF;

          IF UNBOUNDED /= Z THEN
               FAILED ("UNBOUNDED NOT COUNT");
          END IF;
     END;

     RESULT;

END CE3002F;
