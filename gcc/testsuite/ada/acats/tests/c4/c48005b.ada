-- C48005B.ADA

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
-- CHECK THAT AN ALLOCATOR OF THE FORM "NEW T X" ALLOCATES A NEW OBJECT
-- EACH TIME IT IS EXECUTED AND THAT IF X IS AN INDEX CONSTRAINT AND T
-- AN UNCONSTRAINED ARRAY TYPE, THE ALLOCATED OBJECT HAS THE INDEX
-- BOUNDS SPECIFIED BY X.

-- EG  08/10/84

WITH REPORT;

PROCEDURE C48005B IS

     USE REPORT;

BEGIN

     TEST("C48005B","CHECK THAT THE FORM 'NEW T X' ALLOCATES A " &
                    "NEW OBJECT AND THAT IF X IS AN INDEX "      &
                    "CONSTRAINT AND T AN UNCONSTRAINED ARRAY "   &
                    "TYPE, THE ALLOCATED OBJECT HAS THE INDEX "  &
                    "BOUND SPECIFIED BY X");

     DECLARE

          TYPE UA1 IS ARRAY(INTEGER RANGE <>) OF INTEGER;
          TYPE UA2 IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>)
                              OF INTEGER;

          TYPE A_UA1 IS ACCESS UA1;
          TYPE A_UA2 IS ACCESS UA2;

          V_A_UA1 : A_UA1;
          V_A_UA2 : A_UA2;

     BEGIN

          V_A_UA1 := NEW UA1(4 .. 7);
          IF ( V_A_UA1'FIRST /= IDENT_INT(4) OR
               V_A_UA1'LAST  /= IDENT_INT(7) ) THEN
               FAILED("WRONG ARRAY BOUNDS - V_A_UA1");
          END IF;

          V_A_UA2 := NEW UA2(2 .. 3, 4 .. 6);
          IF ( V_A_UA2'FIRST(1) /= IDENT_INT(2) OR
               V_A_UA2'LAST(1)  /= IDENT_INT(3) OR
               V_A_UA2'FIRST(2) /= IDENT_INT(4) OR
               V_A_UA2'LAST(2)  /= IDENT_INT(6) ) THEN
               FAILED("WRONG ARRAY BOUNDS - V_A_UA2");
          END IF;

     END;

     RESULT;

END C48005B;
