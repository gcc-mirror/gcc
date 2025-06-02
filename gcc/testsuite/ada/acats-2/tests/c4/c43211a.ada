-- C43211A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF A BOUND IN A NON-NULL
-- RANGE OF A NON-NULL AGGREGATE DOES NOT BELONG TO THE INDEX SUBTYPE.

-- EG  02/06/84
-- EG  05/08/85
-- EDS 07/15/98    AVOID OPTIMIZATION

WITH REPORT;

PROCEDURE C43211A IS

     USE REPORT;

BEGIN

     TEST("C43211A","CHECK THAT CONSTRAINT_ERROR IS RAISED IF A " &
                    "BOUND IN A NON-NULL RANGE OF A NON-NULL "    &
                    "AGGREGATE DOES NOT BELONG TO THE INDEX "     &
                    "SUBTYPE");

     DECLARE

          SUBTYPE ST IS INTEGER RANGE 4 .. 8;
          TYPE BASE IS ARRAY(ST RANGE <>, ST RANGE <>) OF INTEGER;
          SUBTYPE T IS BASE(5 .. 7, 5 .. 7);

          A    : T;

     BEGIN

CASE_A :  BEGIN

               A := (6 .. 8 => (4 .. 6 => 0));
               IF A /= (6 .. 8 => (4 .. 6 => 0)) THEN
                    FAILED ("CASE A : INCORRECT VALUES");
               END IF;

          EXCEPTION

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED: CASE A");

          END CASE_A;

CASE_B :  BEGIN

               A := (6 .. IDENT_INT(8) =>
                     (IDENT_INT(4) .. 6 => 1));
               IF A /= (6 .. IDENT_INT(8) =>
                        (IDENT_INT(4) .. 6 => 1)) THEN
                    FAILED ("CASE B : INCORRECT VALUES");
               END IF;

          EXCEPTION

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED: CASE B");

          END CASE_B;

CASE_C :  BEGIN

               A := (7 .. 9 => (5 .. 7 => IDENT_INT(2)));
               FAILED ("CONSTRAINT_ERROR NOT RAISED: CASE C " & 
                INTEGER'IMAGE(A(IDENT_INT(7),7)));

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED: CASE C");

          END CASE_C;

CASE_D :  BEGIN

               A := (5 .. 7 => (3 .. 5 => IDENT_INT(3)));
               FAILED ("CONSTRAINT_ERROR NOT RAISED: CASE D " &
                INTEGER'IMAGE(A(7,IDENT_INT(5))));

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED: CASE D");

          END CASE_D;

CASE_E :  BEGIN

               A := (7 .. IDENT_INT(9) => (5 .. 7 => IDENT_INT(4)));
               FAILED ("CONSTRAINT_ERROR NOT RAISED: CASE E " &
                INTEGER'IMAGE(A(IDENT_INT(7),7)));

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("CASE E : EXCEPTION RAISED");

          END CASE_E;

CASE_F :  BEGIN

               A := (5 .. 7 => (IDENT_INT(3) .. 5 => IDENT_INT(5)));
               FAILED ("CONSTRAINT_ERROR NOT RAISED: CASE F " &
                INTEGER'IMAGE(A(7,IDENT_INT(5))));

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED: CASE F");

          END CASE_F;

CASE_G :  BEGIN

               A := (7 .. 8 => (5 .. 7 => IDENT_INT(6)),
              9 => (5 .. 7 => IDENT_INT(6)));
               FAILED ("CONSTRAINT_ERROR NOT RAISED: CASE G " &
                INTEGER'IMAGE(A(7,IDENT_INT(7))));

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED: CASE G");

          END CASE_G;

     END;

     RESULT;

END C43211A;
