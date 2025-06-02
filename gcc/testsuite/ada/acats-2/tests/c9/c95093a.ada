-- C95093A.ADA

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
-- CHECK THAT THE DEFAULT EXPRESSIONS OF FORMAL PARAMETERS ARE EVALUATED
-- EACH TIME THEY ARE NEEDED.

-- GLH 7/2/85

WITH REPORT; USE REPORT;

PROCEDURE C95093A IS
BEGIN

     TEST ("C95093A", "CHECK THAT THE DEFAULT EXPRESSION IS " &
                      "EVALUATED EACH TIME IT IS NEEDED");

     DECLARE

          X : INTEGER := 1;

          FUNCTION F RETURN INTEGER IS
          BEGIN
               X := X + 1;
               RETURN X;
          END F;

          TASK T1 IS
               ENTRY E1 (X, Y : INTEGER := F);
          END T1;

          TASK BODY T1 IS
          BEGIN

               ACCEPT E1 (X, Y : INTEGER := F) DO
                    IF X = Y OR Y /= 2 THEN
                         FAILED ("DEFAULT NOT EVALUATED CORRECTLY - " &
                                 "1, X =" & INTEGER'IMAGE(X) &
                                 ", Y =" & INTEGER'IMAGE(Y));
                    END IF;
               END E1;

               ACCEPT E1 (X, Y : INTEGER := F) DO
                    IF X = Y OR
                       NOT ((X = 3 AND Y = 4) OR
                            (X = 4 AND Y = 3)) THEN
                         FAILED ("DEFAULT NOT EVALUATED CORRECTLY - " &
                                 "2, X =" & INTEGER'IMAGE(X) &
                                 ", Y =" & INTEGER'IMAGE(Y));
                    END IF;
               END E1;

          END T1;

     BEGIN

          COMMENT ("FIRST CALL");
          T1.E1 (3);

          COMMENT ("SECOND CALL");
          T1.E1;

     END;

     RESULT;

END C95093A;
