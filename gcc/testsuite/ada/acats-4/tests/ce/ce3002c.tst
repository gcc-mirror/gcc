-- CE3002C.TST

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
--     CHECK THAT FIELD IS A SUBTYPE OF INTEGER, FIELD'FIRST = 0, AND
--     FIELD'LAST HAS A SPECIFIED IMPLEMENTATION-DEPENDENT VALUE.

-- HISTORY:
--     SPS 09/30/82
--     SPS 11/09/82
--     JBG 03/16/83
--     JLH 08/07/87  REVISED VALUES USED IN INTEGER AND FIELD TO THE
--                   INTEGER VALUE 1.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE CE3002C IS
BEGIN

     TEST ("CE3002C", "CHECK THAT FIELD IS A SUBTYPE OF INTEGER AND " &
                      "FIELD'FIRST = 0");

     DECLARE
          A : INTEGER;
          B : FIELD;
     BEGIN
          IF FIELD'FIRST /= IDENT_INT (0) THEN
               FAILED ("FIELD'FIRST NOT 0; IS" &
                       FIELD'IMAGE(FIELD'FIRST));
          END IF;

          IF FIELD'LAST /= $FIELD_LAST THEN
               FAILED ("FIELD'LAST NOT $FIELD_LAST; IS" &
                       FIELD'IMAGE(FIELD'LAST));
          END IF;

          A := IDENT_INT (1);
          B := A;
          B := IDENT_INT (1);
          A := B;
     END;

     RESULT;

END CE3002C;
