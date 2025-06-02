-- C24002D.ADA

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
-- CHECK THAT LOWER CASE E MAY BE USED IN INTEGER LITERALS, FLOATING POINT 
-- LITERALS, AND FIXED POINT LITERALS.
-- CHECK THAT THESE NUMERIC LITERALS YIELD THE CORRECT VALUES.

-- WMC 03/16/92  CONSOLIDATION OF C24002A.ADA, C24002B.ADA, C24002C.ADA

WITH REPORT;

PROCEDURE C24002D IS

        USE REPORT;

BEGIN
        TEST("C24002D", "CHECK THAT LOWER CASE E WORKS IN INTEGER, " &
                        "FLOATING POINT, AND FIXED POINT LITERALS, " &
                        "AND THAT THESE NUMERIC LITERALS YIELD THE " &
                        "CORRECT VALUES");

        -- Integer Literals
        DECLARE
           X,Y : INTEGER;
        BEGIN
           X := 12e1;
           Y := 16#E#e1;

           IF (X /= 120) OR (Y /= 224) THEN
                FAILED("INCORRECT HANDLING OF LOWER CASE E " &
                       "IN INTEGER LITERALS");
           END IF;
        END;


        -- Floating Point Literal
        DECLARE
           X : FLOAT;
        BEGIN
           X := 16#F.FF#e+2;

           IF (X /= 4095.0) THEN
                FAILED("INCORRECT HANDLING OF LOWER CASE E " &
                       "IN BASED FLOATING POINT LITERALS");
           END IF;
        END;


        -- Fixed Point Literal
        DECLARE 
           TYPE FIXED IS DELTA 0.1 RANGE 0.0 .. 300.0;
           X : FIXED;
        BEGIN
           X := 16#F.F#e1;

           IF (X /= 255.0) THEN
                FAILED("INCORRECT HANDLING OF LOWER CASE E " &
                       "IN BASED FIXED POINT LITERALS");
           END IF;
        END;

        RESULT;

END C24002D;
