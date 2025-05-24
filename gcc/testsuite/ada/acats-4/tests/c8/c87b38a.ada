-- C87B38A.ADA

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
    
-- IN A QUALIFIED EXPRESSION, THE OPERAND MUST HAVE THE SAME TYPE
-- AS THE BASE TYPE OF THE TYPEMARK.
  
-- TRH  13 SEPT 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B38A IS
 
     SUBTYPE BOOL IS BOOLEAN;
     TYPE YES  IS NEW BOOLEAN RANGE TRUE  .. TRUE;
     TYPE NO   IS NEW BOOLEAN RANGE FALSE .. FALSE;
     TYPE BIT  IS NEW BOOLEAN;
     TYPE LIT  IS (FALSE, TRUE);   
     TYPE FLAG IS (PASS,  FAIL);
 
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
               FAILED ("RESOLUTION INCORRECT FOR OVERLOADED " &
                       " OPERANDS OF QUALIFIED EXPRESSIONS");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION F IS NEW F1 (LIT,     FALSE, FAIL);
     FUNCTION F IS NEW F1 (BIT,     TRUE,  FAIL);
     FUNCTION F IS NEW F1 (BOOLEAN, TRUE,  PASS);
     FUNCTION F IS NEW F1 (YES,     TRUE,  FAIL);
     FUNCTION F IS NEW F1 (NO,      FALSE, FAIL);
    
BEGIN
     TEST ("C87B38A","OVERLOADED OPERANDS IN QUALIFIED EXPRESSIONS ");
 
     DECLARE 
          B : BOOL;
 
     BEGIN
          B := BOOL' (F);
          B := BOOL' ((NOT F) OR ELSE (F AND THEN F));
     END;
 
     RESULT;
END C87B38A;
