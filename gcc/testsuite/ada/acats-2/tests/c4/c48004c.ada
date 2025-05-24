-- C48004C.ADA

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
-- CHECK THAT THE FORM "NEW T" IS PERMITTED IF T IS AN UNCONSTRAINED
-- RECORD, PRIVATE, OR LIMITED TYPE WHOSE DISCRIMINANTS HAVE DEFAULT
-- VALUES.

-- EG  08/03/84

WITH REPORT;

PROCEDURE C48004C IS

     USE REPORT;

BEGIN

     TEST("C48004C","CHECK THAT THE FORM 'NEW T' IS PERMITTED IF "   &
                    "T IS AN UNCONSTRAINED RECORD, PRIVATE, OR "     &
                    "LIMITED TYPE WHOSE DISCRIMINANTS HAVE DEFAULT " &
                    "VALUES");

     DECLARE

          TYPE  UR(A : INTEGER := 1; B : INTEGER := 2)  IS
               RECORD
                    C : INTEGER := 7;
               END RECORD;

          PACKAGE  P  IS

               TYPE UP(A : INTEGER := 12; B : INTEGER := 13) IS PRIVATE;
               TYPE UL(A, B : INTEGER := 1) IS LIMITED PRIVATE;

          PRIVATE

               TYPE UP(A : INTEGER := 12; B : INTEGER := 13) IS
                    RECORD
                         Q : INTEGER;
                    END RECORD;
               TYPE UL(A, B : INTEGER := 1) IS
                    RECORD
                         Q : INTEGER;
                    END RECORD;

          END P;
     
          USE P;

          TYPE A_UR IS ACCESS UR;
          TYPE A_UP IS ACCESS UP;
          TYPE A_UL IS ACCESS UL;

          V_UR : A_UR;
          V_UP : A_UP;
          V_UL : A_UL;

     BEGIN

          V_UR := NEW UR;
          IF ( V_UR.A /= IDENT_INT(1) OR V_UR.B /= 2 OR
               V_UR.C /= 7 ) THEN 
               FAILED("WRONG VALUES - UR");
          END IF;

          V_UP := NEW UP;
          IF ( V_UP.A /= IDENT_INT(12) OR V_UP.B /= 13 ) THEN
               FAILED("WRONG VALUES - UP");
          END IF;
          
          V_UL := NEW UL;
          IF ( V_UL.A /= IDENT_INT(1) OR V_UL.B /= 1 ) THEN
               FAILED("WRONG VALUES - UL");
          END IF;

     END;

     RESULT;

END C48004C;
