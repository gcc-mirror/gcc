-- CE3002B.TST

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
--     CHECK THAT COUNT IS A VISIBLE TYPE, THAT COUNT'FIRST IS 0,
--     THAT POSITIVE_COUNT IS A SUBTYPE OF COUNT, THAT
--     POSITIVE_COUNT'FIRST IS 1, THAT POSITIVE_COUNT'LAST
--     EQUALS COUNT'LAST, AND COUNT'LAST HAS A SPECIFIED
--     IMPLEMENTATION-DEPENDENT VALUE.

-- HISTORY:
--     SPS 09/30/82
--     SPS 11/09/82
--     JBG 03/16/83
--     JLH 08/07/87  REVISED VALUES USED IN COUNT AND POSITIVE_COUNT
--                   TO THE INTEGER VALUE 1.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3002B IS
BEGIN

     TEST ("CE3002B", "CHECK THAT COUNT IS VISIBLE, COUNT'FIRST IS " &
                      "0, POSITIVE_COUNT IS A SUBTYPE OF COUNT, " &
                      "POSITIVE_COUNT'FIRST IS 1, POSITIVE_COUNT'" &
                      "LAST EQUALS COUNT'LAST, AND COUNT'LAST " &
                      "HAS A SPECIFIED VALUE");

     DECLARE
          X : COUNT;
          A : POSITIVE_COUNT;
     BEGIN
          IF COUNT'FIRST /= COUNT(IDENT_INT (0)) THEN
               FAILED ("COUNT'FIRST NOT 0; IS" &
                       COUNT'IMAGE(COUNT'FIRST));
          END IF;

          IF POSITIVE_COUNT'FIRST /= POSITIVE_COUNT (IDENT_INT (1)) THEN
               FAILED ("POSITIVE_COUNT'FIRST NOT 1; IS" &
                       COUNT'IMAGE(POSITIVE_COUNT'FIRST));
          END IF;

          IF POSITIVE_COUNT'LAST /= COUNT'LAST THEN
               FAILED ("POSITIVE_COUNT'LAST NOT EQUAL COUNT'LAST");
          END IF;

          IF COUNT'LAST /= $COUNT_LAST THEN
               FAILED ("COUNT'LAST NOT $COUNT_LAST; IS" &
                       COUNT'IMAGE(COUNT'LAST));
          END IF;

          X := POSITIVE_COUNT (IDENT_INT (1));
          A := X;
          A := COUNT (IDENT_INT (1));
          X := A;
     END;

     RESULT;

END CE3002B;
