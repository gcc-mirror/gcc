-- C43205B.ADA

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
-- 'FIRST OF THE INDEX SUBTYPE WHEN THE POSITIONAL AGGREGATE IS USED AS:

--   B) AN ACTUAL PARAMETER IN A GENERIC INSTANTIATION, AND THE FORMAL
--      PARAMETER IS UNCONSTRAINED.

-- EG  01/26/84

WITH REPORT;

PROCEDURE C43205B IS

     USE REPORT;

BEGIN

     TEST("C43205B", "CASE B : UNCONSTRAINED ARRAY FORMAL GENERIC " &
                      "PARAMETER");

     BEGIN

CASE_B :  DECLARE

               SUBTYPE STB IS INTEGER RANGE IDENT_INT(-8) .. -5;
               TYPE TB IS ARRAY (STB RANGE <>) OF INTEGER;

               GENERIC
                    B1 : TB;
               PROCEDURE PROC1;

               PROCEDURE PROC1 IS
               BEGIN
                    IF B1'FIRST /= -8 THEN
                         FAILED ("CASE B : LOWER BOUND INCORRECTLY " &
                                 "GIVEN BY 'FIRST");
                    ELSIF B1'LAST /= IDENT_INT(-5) THEN
                         FAILED ("CASE B : UPPER BOUND INCORRECTLY " &
                                 "GIVEN BY 'LAST");
                    ELSIF B1 /= (7, 6, 5, 4) THEN
                         FAILED ("CASE B : ARRAY DOES NOT " &
                                 "CONTAIN THE CORRECT VALUES");
                    END IF;
               END;

               PROCEDURE PROC2 IS NEW PROC1 ((7, 6, IDENT_INT(5), 4));

          BEGIN

               PROC2;

          END CASE_B;

     END;

     RESULT;

END C43205B;
