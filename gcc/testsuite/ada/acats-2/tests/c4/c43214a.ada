-- C43214A.ADA

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
-- FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM (F..G => ""), CHECK
-- THAT CONSTRAINT_ERROR IS RAISED IF F..G IS NON-NULL AND
-- F OR G DO NOT BELONG TO THE INDEX SUBTYPE.

-- EG  02/10/1984
-- JBG 12/6/84
-- EDS 07/15/98     AVOID OPTIMIZATION

WITH REPORT;

PROCEDURE C43214A IS

     USE REPORT;

BEGIN

     TEST("C43214A", "FOR A MULTIDIMENSIONAL AGGREGATE OF THE FORM " &
                     "(F..G => """"), CHECK THAT CONSTRAINT ERROR "  &
                     "IS RAISED IF F..G IS NON-NULL AND NOT IN THE " &
                     "INDEX SUBTYPE");

     DECLARE

          SUBTYPE STA IS INTEGER RANGE 4 .. 7;
          TYPE TA IS ARRAY(STA RANGE 5 .. 6, 
                           STA RANGE 6 .. IDENT_INT(4)) OF CHARACTER;

          A : TA := (5 .. 6 => "");

     BEGIN

CASE_A :  BEGIN

               IF (6 .. IDENT_INT(8) => "") = A THEN
                    FAILED ("CASE A : CONSTRAINT_ERROR NOT RAISED");
               END IF;
               FAILED ("CASE A : CONSTRAINT_ERROR NOT RAISED - 2");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("CASE A : WRONG EXCEPTION RAISED");

          END CASE_A;

CASE_B :  BEGIN

               A := (IDENT_INT(3) .. 4 => "");
               FAILED ("CASE B : CONSTRAINT_ERROR NOT RAISED");
               BEGIN
                  FAILED("ATTEMPT TO USE A " &
                         CHARACTER'VAL(IDENT_INT(CHARACTER'POS(
                            A(A'FIRST(1), A'FIRST(2)) ))) );
               EXCEPTION
                  WHEN OTHERS =>
                     FAILED("CONSTRAINT_ERROR NOT RAISED AT PROPER PLACE");
               END;

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;

               WHEN OTHERS =>
                    FAILED ("CASE B : WRONG EXCEPTION RAISED");

          END CASE_B;

     END;

     RESULT;

END C43214A;
