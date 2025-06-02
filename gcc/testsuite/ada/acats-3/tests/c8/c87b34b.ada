-- C87B34B.ADA

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
    
-- THE "IN" (OR MEMBERSHIP) OPERATOR OF THE FORM:  X IN L .. R
-- REQUIRES THE OPERANDS X, L AND R TO BE OF THE SAME SCALAR TYPE.
  
-- TRH  19 JULY 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B34B IS
 
     TYPE FLAG IS (PASS, FAIL);
  
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
             FAILED ("RESOLUTION INCORRECT FOR 'IN' MEMBERSHIP TEST");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION X IS NEW F1 (FLOAT,     2.0, PASS);
     FUNCTION L IS NEW F1 (FLOAT,    -1.0, PASS);
     FUNCTION R IS NEW F1 (FLOAT,     1.0, PASS);
     FUNCTION X IS NEW F1 (INTEGER,     5, FAIL);
     FUNCTION L IS NEW F1 (INTEGER,     1, FAIL);
     FUNCTION L IS NEW F1 (CHARACTER, 'A', FAIL);
     FUNCTION R IS NEW F1 (CHARACTER, 'E', FAIL);
     FUNCTION X IS NEW F1 (BOOLEAN,  TRUE, FAIL);
     FUNCTION R IS NEW F1 (BOOLEAN,  TRUE, FAIL);
    
BEGIN
     TEST ("C87B34B","OVERLOADED MEMBERSHIP OPERANDS");
 
     IF X IN L .. R THEN 
        FAILED ("RESOLUTION INCORRECT FOR MEMBERSHIP OPERATOR");
     END IF;
 
     RESULT;
END C87B34B;
