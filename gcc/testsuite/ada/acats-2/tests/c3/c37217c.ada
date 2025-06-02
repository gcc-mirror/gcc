-- C37217C.ADA

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
--     CHECK WHETHER THE OPTIONAL COMPATIBILITY CHECK IS
--     PERFORMED WHEN A DISCRIMINANT CONSTRAINT IS GIVEN FOR AN ACCESS
--     TYPE - WHEN THERE IS A "LOOP" IN THE DESIGNATED TYPE'S FULL
--     DECLARATION.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C37217C IS

BEGIN  --C37217C BODY
     TEST ("C37217C", "CHECK WHETHER THE OPTIONAL COMPATIBILITY " &
                      "CHECK IS PERFORMED WHEN A DISCRIMINANT " &
                      "CONSTRAINT IS GIVEN FOR AN ACCESS TYPE " &
                      "- WHEN THERE IS A ""LOOP"" IN THE DESIGNATED " &
                      "TYPE'S FULL DECLARATION");

     BEGIN
          DECLARE
               TYPE R1(D1 : INTEGER);
               TYPE R2(D2 : INTEGER);
               TYPE R3(D3 : POSITIVE);

               TYPE ACC_R1 IS ACCESS R1;
               TYPE ACC_R2 IS ACCESS R2;
               TYPE ACC_R3 IS ACCESS R3;

               TYPE R1(D1 : INTEGER) IS
                    RECORD
                         C1 : ACC_R2(D1);
                    END RECORD;

               TYPE R2(D2 : INTEGER) IS
                    RECORD
                         C2 : ACC_R3(D2);
                    END RECORD;

               TYPE R3(D3 : POSITIVE) IS
                    RECORD
                         C3 : ACC_R1(D3);
                    END RECORD;

                X1 : ACC_R1(IDENT_INT(0));

          BEGIN
               COMMENT("OPTIONAL COMPATIBILITY CHECK NOT PERFORMED");

               X1 := NEW R1'(D1 =>IDENT_INT(0),
                             C1 => NEW R2'(D2 => IDENT_INT(0),
                                           C2 => NEW R3(IDENT_INT(0))));

               FAILED("CONSTRAINT_ERROR NOT RAISED");

               IF IDENT_INT(X1.C1.C2.D3) /= IDENT_INT(0) THEN
                    COMMENT("THIS LINE SHOULD NOT PRINT OUT");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                           "VARIABLE USE - LOOPED");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT("OPTIONAL COMPATIBILITY CHECK PERFORMED");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                      "VARIABLE DECLARATION - LOOPED");
     END;

     RESULT;

END C37217C;  -- BODY
