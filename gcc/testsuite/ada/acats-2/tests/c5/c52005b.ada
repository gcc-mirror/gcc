-- C52005B.ADA

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
-- CHECK THAT THE CONSTRAINT_ERROR EXCEPTION IS RAISED
--    WHEN A STATIC EXPRESSION VALUE IS OUTSIDE THE STATIC RANGE
--    OF FLOATING POINT ASSIGNMENTS.

-- DCB 2/6/80
-- JRK 7/21/80
-- SPS 3/21/83

WITH REPORT;
PROCEDURE C52005B IS

     USE REPORT;

BEGIN
     TEST ("C52005B", "CHECK THAT CONSTRAINT_ERROR EXCEPTION IS RAISED"
         & " ON STATIC OUT OF RANGE FLOATING POINT ASSIGNMENTS");

-------------------------

     DECLARE
          TYPE FLT IS DIGITS 3 RANGE 0.0 .. 5.0E2;
          FL1 : FLT RANGE 0.0 .. 100.0 := 50.0;

     BEGIN
          FL1 := 101.0;

          FAILED ("EXCEPTION NOT RAISED FOR OUT OF RANGE FLT1 PT " &
                  "ASSIGNMENT");

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          IF FL1 /= 50.0 THEN
               FAILED ("VALUE ALTERED BEFORE FLT1 PT RANGE EXCEPTION");
          END IF;

     END;

-------------------------

     DECLARE
          TYPE FLT IS DIGITS 3 RANGE 0.0 .. 5.0E2;
          FL2 : FLT RANGE 0.0 .. 100.0 := 50.0;


     BEGIN
          FL2 := 100.0;

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          FAILED ("EXCEPTION RAISED ON LEGAL FLOATING1 PT" &
                  "ASSIGNMENT");

     END;

-------------------------

     DECLARE
          FL1 : FLOAT RANGE 0.0 .. 100.0 := 50.0;

     BEGIN
          FL1 := -0.001;

          FAILED ("EXCEPTION NOT RAISED FOR OUT OF RANGE FLTG PT " &
                  "ASSIGNMENT");

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          IF FL1 /= 50.0 THEN
               FAILED ("VALUE ALTERED BEFORE FLTG PT RANGE EXCEPTION");
          END IF;

     END;

-------------------------

     DECLARE
          FL2 : FLOAT RANGE 0.0 .. 100.0 := 50.0;

     BEGIN
          FL2 := 0.0;

     EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          FAILED ("EXCEPTION RAISED ON LEGAL FLOATING PT ASSNMT");

     END;

----------------------

     RESULT;
END C52005B;
