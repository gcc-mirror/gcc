-- C62006A.ADA

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
-- CHECK THAT THE DISCRIMINANTS OF AN OUT FORMAL PARAMETER, AS WELL AS
-- THE DISCRIMINANTS OF THE SUBCOMPONENTS OF AN OUT FORMAL PARAMETER,
-- MAY BE READ INSIDE THE PROCEDURE.

-- SPS 2/17/84

WITH REPORT; USE REPORT;
PROCEDURE C62006A IS
BEGIN

     TEST ("C62006A", "CHECK THAT THE DISCRIMINANTS OF AN OUT FORMAL " &
           "PARAMETER CAN BE READ INSIDE THE PROCEDURE");

     DECLARE

          TYPE R1 (D1 : INTEGER) IS RECORD
               NULL;
          END RECORD;

          TYPE R2 (D2 : POSITIVE) IS RECORD
               C : R1 (2);
          END RECORD;

          R : R2 (5);

          PROCEDURE P (REC : OUT R2) IS
          BEGIN

               IF REC.D2 /= 5 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT OF" &
                            " OUT PARAMETER");
               END IF;

               IF REC.C.D1 /= 2 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
                            " OF THE SUBCOMPONENT OF AN OUT PARAMETER");
               END IF;
          END P;

     BEGIN
          P (R);
     END;

     RESULT;

END C62006A;
