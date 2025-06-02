-- C85007A.ADA

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
-- CHECK THAT THE DISCRIMINANTS OF A RENAMED OUT FORMAL PARAMETER, AS
-- WELL AS THE DISCRIMINANTS OF THE RENAMED SUBCOMPONENTS OF AN OUT
-- FORMAL PARAMETER, MAY BE READ INSIDE THE PROCEDURE.

-- SPS 02/17/84 (SEE C62006A-B.ADA)
-- EG  02/21/84

WITH REPORT; USE REPORT;

PROCEDURE C85007A IS

BEGIN

     TEST ("C85007A", "CHECK THAT THE DISCRIMINANTS OF A RENAMED OUT " &
           "FORMAL PARAMETER CAN BE READ INSIDE THE PROCEDURE");

     DECLARE

          TYPE R1 (D1 : INTEGER) IS RECORD
               NULL;
          END RECORD;

          TYPE R2 (D2 : POSITIVE) IS RECORD
               C : R1 (2);
          END RECORD;

          SUBTYPE R1_2 IS R1(2);

          R : R2 (5);

          PROCEDURE PROC (REC : OUT R2) IS

               REC1 : R2   RENAMES REC;
               REC2 : R1_2 RENAMES REC.C;
               REC3 : R2   RENAMES REC1;
               REC4 : R1_2 RENAMES REC1.C;
               REC5 : R1_2 RENAMES REC4;

          BEGIN

               IF REC1.D2 /= 5 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT OF" &
                            " A RENAMED OUT PARAMETER");
               END IF;

               IF REC1.C.D1 /= 2 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
                            "OF THE SUBCOMPONENT OF A RENAMED OUT "  &
                            "PARAMETER");
               END IF;

               IF REC2.D1 /= 2 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
                            "OF A RENAMED SUBCOMPONENT OF AN OUT "   &
                            "PARAMETER");
               END IF;

               IF REC3.D2 /= 5 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT OF" &
                            " A RENAME OF A RENAMED OUT PARAMETER");
               END IF;

               IF REC3.C.D1 /= 2 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
                            "OF THE SUBCOMPONENT OF A RENAME OF A "  &
                            "RENAMED OUT PARAMETER");
               END IF;

               IF REC4.D1 /= 2 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
                            "OF A RENAMED SUBCOMPONENT OF A RENAMED" &
                            " OUT PARAMETER");
               END IF;

               IF REC5.D1 /= 2 THEN
                    FAILED ("UNABLE TO CORRECTLY READ DISCRIMINANT " &
                            "OF A RENAME OF RENAMED SUBCOMPONENT OF" &
                            " A RENAMED OUT PARAMETER");
               END IF;

          END PROC;

     BEGIN

          PROC (R);

     END;

     RESULT;

END C85007A;
