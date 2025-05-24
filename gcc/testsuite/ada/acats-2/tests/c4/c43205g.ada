-- C43205G.ADA

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
-- CHECK THAT THE BOUNDS OF A POSITIONAL AGGREGATE ARE DETERMINED
-- CORRECTLY. IN PARTICULAR, CHECK THAT THE LOWER BOUND IS GIVEN BY
-- THE LOWER BOUND OF THE APPLICABLE INDEX CONSTRAINT WHEN THE
-- POSITIONAL AGGREGATE IS USED AS:

-- AN ACTUAL PARAMETER IN A SUBPROGRAM, AND THE
-- FORMAL PARAMETER IS CONSTRAINED.

-- EG  01/27/84

WITH REPORT;

PROCEDURE C43205G IS

     USE REPORT;

BEGIN

     TEST("C43205G", "SUBPROGRAM WITH CONSTRAINED " &
                     "ONE-DIMENSIONAL ARRAY FORMAL PARAMETER");

     BEGIN

CASE_G :  BEGIN

     CASE_G1 : DECLARE

                    TYPE TA IS ARRAY (IDENT_INT(11) .. 15) OF INTEGER;

                    PROCEDURE PROC1 (A : TA) IS
                    BEGIN
                         IF A'FIRST /= 11 THEN
                              FAILED ("CASE A1 : LOWER BOUND " &
                                      "INCORRECT");
                         ELSIF A'LAST /= 15 THEN
                              FAILED ("CASE A1 : UPPER BOUND " &
                                      "INCORRECT");
                         ELSIF A /= (6, 7, 8, 9, 10) THEN
                              FAILED ("CASE A1 : ARRAY DOES NOT " &
                                     "CONTAIN THE CORRECT VALUES");
                         END IF;
                    END;

               BEGIN

                    PROC1 ((6, 7, 8, IDENT_INT(9), 10));

               END CASE_G1;

     CASE_G2 : DECLARE

                    TYPE TA IS ARRAY (11 .. 12, 
                                      IDENT_INT(10) .. 11) OF INTEGER;

                    PROCEDURE PROC1 (A : TA) IS
                    BEGIN
                         IF A'FIRST(1) /= 11 OR A'FIRST(2) /= 10 THEN
                              FAILED ("CASE A2 : LOWER BOUND " &
                                      "INCORRECT");
                         ELSIF A'LAST(1) /= 12 OR A'LAST(2) /= 11 THEN
                              FAILED ("CASE A2 : UPPER BOUND " &
                                      "INCORRECT");
                         ELSIF A /= ((1, 2), (3, 4)) THEN
                              FAILED ("CASE A2 : ARRAY DOES NOT " &
                                      "CONTAIN THE CORRECT VALUES");
                         END IF;
                    END;

               BEGIN

                    PROC1 (((1, 2), (3, 4)));

               END CASE_G2;

          END CASE_G;

     END;

     RESULT;

END C43205G;
