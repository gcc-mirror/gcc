-- CE3002D.ADA

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
-- CHECK THAT NUMBER_BASE IS A SUBTYPE OF INTEGER, WITH
-- NUMBER_BASE'FIRST EQUAL 2 AND NUMBER_BASE'LAST EQUAL 16.

-- SPS 10/1/82

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3002D IS
BEGIN

     TEST ("CE3002D", "CHECK THAT NUMBER_BASE IS A SUBTYPE " &
                      "OF INTEGER WITH NUMBER_BASE'FIRST = 2 " &
                      "AND NUMBER_BASE'LAST = 16");

     DECLARE
          X : INTEGER;
          Y : NUMBER_BASE;
     BEGIN
          IF NUMBER_BASE'FIRST /= IDENT_INT (2) THEN
               FAILED ("NUMBER_BASE'FIRST NOT 2");
          END IF;

          IF NUMBER_BASE'LAST /= IDENT_INT (16) THEN
               FAILED ("NUMBER_BASE'LAST NOT 16");
          END IF;

          X := IDENT_INT (3);
          Y := X;
          Y := IDENT_INT (8);
          X := Y;
     END;

RESULT;
END CE3002D;
