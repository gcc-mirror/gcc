-- C87B04A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- IN A RANGE CONSTRAINT OF A SUBTYPE INDICATION, THE EXPRESSIONS
-- FOR THE LOWER AND UPPER BOUNDS MUST BE COMPATIBLE WITH THE SUBTYPE'S
-- EXPLICIT TYPEMARK.
  
-- TRH  28 JUNE 82
-- JBG 3/8/84

WITH REPORT; USE REPORT;
PROCEDURE C87B04A IS
  
     TYPE AGE    IS NEW INTEGER RANGE 1 .. 120;
     TYPE BASE10 IS NEW INTEGER RANGE 0 .. 9;
     
     FUNCTION F1 RETURN AGE IS
     BEGIN
          RETURN 18;
     END F1;

     FUNCTION F1 RETURN INTEGER IS
     BEGIN
          FAILED ("RESOLUTION INCORRECT - RANGE CONSTRAINT OF " &
                  "SUBTYPE INDICATION");
          RETURN 0;
     END F1;
    
     FUNCTION "+" (X : INTEGER) RETURN BASE10 IS
     BEGIN
          RETURN 1;
     END "+";

     FUNCTION "+" (X : INTEGER) RETURN INTEGER IS
     BEGIN
          FAILED ("RESOLUTION INCORRECT - RANGE CONSTRAINT OF " &
                  "SUBTYPE INDICATION");
          RETURN -X;
     END "+";
  
BEGIN
     TEST ("C87B04A","OVERLOADED EXPRESSIONS IN RANGE CONTRAINTS" &
           " OF SUBTYPE INDICATIONS");
   
     DECLARE
          SUBTYPE MINOR IS AGE RANGE 1 .. F1;
  
     BEGIN
          FOR I IN BASE10 RANGE +(INTEGER'(0)) .. 0 LOOP
               FAILED ("RESOLUTION INCORRECT - SUBTYPE INDICATION " &
                       " IN LOOP CONSTRUCT");
          END LOOP;
     END;
    
     RESULT;
END C87B04A;
