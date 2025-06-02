-- C87B47A.ADA

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
-- ACTUAL PARAMETERS MUST MATCH THE EXPLICIT TYPEMARK OF THE
-- PARAMETER.
  
-- TRH  8 AUG 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B47A IS
 
     TYPE FLAG IS (PASS, FAIL);
     
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
             FAILED ("ACTUAL PARAMETER MUST MATCH PARAMETER TYPE");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION F IS NEW F1 (FLOAT,     2.0, PASS);
     FUNCTION F IS NEW F1 (INTEGER,     5, FAIL);
     FUNCTION F IS NEW F1 (BOOLEAN,  TRUE, FAIL);
     FUNCTION F IS NEW F1 (DURATION,  1.0, FAIL);
     FUNCTION F IS NEW F1 (CHARACTER, 'E', FAIL);
    
BEGIN
     TEST ("C87B47A","OVERLOADED ACTUAL PARAMETERS");
    
     DECLARE
          PROCEDURE P (X : FLOAT) IS
          BEGIN
               NULL;
          END P;
   
     BEGIN
          P (F);
          P (X => F);
     END;
 
     RESULT;
END C87B47A;
