-- C74207B.ADA

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
-- CHECK THAT 'CONSTRAINED CAN BE APPLIED AFTER THE FULL DECLARATION OF
-- A PRIVATE TYPE THAT IS DERIVED FROM A PRIVATE TYPE.

-- BHS 6/18/84

WITH REPORT;
USE REPORT;
PROCEDURE C74207B IS
BEGIN
     TEST ("C74207B", "AFTER THE FULL DECLARATION OF A PRIVATE " &
                      "TYPE DERIVED FROM A PRIVATE TYPE, " &
                      "'CONSTRAINED MAY BE APPLIED");

     DECLARE
          PACKAGE P1 IS
               TYPE PREC (D : INTEGER) IS PRIVATE;
               TYPE P IS PRIVATE;
          PRIVATE
               TYPE PREC (D : INTEGER) IS RECORD
                         NULL; 
                    END RECORD;
               TYPE P IS NEW INTEGER;
          END P1;

          PACKAGE P2 IS
               TYPE LP1 IS LIMITED PRIVATE;
               TYPE LP2 IS LIMITED PRIVATE;
          PRIVATE
               TYPE LP1 IS NEW P1.PREC(3);
               TYPE LP2 IS NEW P1.P;
               B1 : BOOLEAN := LP1'CONSTRAINED;
               B2 : BOOLEAN := LP2'CONSTRAINED;
          END P2;

          PACKAGE BODY P2 IS
          BEGIN
               IF NOT IDENT_BOOL(B1) THEN
                    FAILED ("WRONG VALUE FOR LP1'CONSTRAINED");
               END IF;
               IF NOT IDENT_BOOL(B2) THEN
                    FAILED ("WRONG VALUE FOR LP2'CONSTRAINED");
               END IF;
          END P2;

     BEGIN
          NULL;
     END;

     RESULT;

END C74207B;
