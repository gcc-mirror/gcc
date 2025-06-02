-- C87B28A.ADA

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
-- THE TYPE OF THE LITERAL "NULL" MUST BE DETERMINED FROM THE FACT 
-- THAT "NULL" IS A VALUE OF AN ACCESS TYPE.
  
-- TRH  13 AUG 82
-- JRK 2/2/84

WITH REPORT; USE REPORT;
   
PROCEDURE C87B28A IS
 
     ERR : BOOLEAN := FALSE;
 
     TYPE A2 IS ACCESS BOOLEAN;
     TYPE A3 IS ACCESS INTEGER;
     TYPE A1 IS ACCESS A2;
 
     FUNCTION F RETURN A1 IS
     BEGIN 
          RETURN NEW A2;
     END F;
 
     FUNCTION F RETURN A2 IS
     BEGIN
          ERR := TRUE;
          RETURN NEW BOOLEAN;
     END F;
 
     FUNCTION F RETURN A3 IS
     BEGIN
          ERR := TRUE;
          RETURN (NEW INTEGER);
     END F;
 
BEGIN
     TEST ("C87B28A", "OVERLOADING OF THE ACCESS TYPE LITERAL 'NULL'");
    
     F.ALL := NULL;
 
     IF ERR THEN
          FAILED ("RESOLUTION INCORRECT FOR THE ACCESS TYPE LITERAL " &
                  "'NULL'");
     END IF;
 
     RESULT;
END C87B28A;
