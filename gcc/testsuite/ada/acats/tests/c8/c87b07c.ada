-- C87B07C.ADA

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
-- FOR THE ATTRIBUTE OF THE FORM T'VALUE (X), THE OPERAND X MUST 
-- BE OF THE PREDEFINED TYPE STRING. THE RESULT IS OF TYPE T.
  
-- TRH  13 SEPT 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B07C IS
 
     TYPE CHAR IS NEW CHARACTER;
     TYPE LITS IS (' ', '+', '1');
     TYPE WORD IS ARRAY (POSITIVE RANGE 1..4) OF CHARACTER;
     TYPE LINE IS ARRAY (POSITIVE RANGE 1..4) OF CHAR;
     TYPE LIST IS ARRAY (POSITIVE RANGE 1..4) OF LITS;
     TYPE STR  IS ARRAY (POSITIVE RANGE 1..4) OF STRING (1 .. 1);
     TYPE STR2 IS NEW STRING (1..4);
     TYPE FLAG IS (PASS, FAIL);
     SUBTYPE MY_STRING IS STRING (1..4);
     
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
               FAILED ("THE 'VALUE' ATTRIBUTE TAKES AN OPERAND" &
                       " OF THE TYPE PREDEFINED STRING");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION F IS NEW F1 (STR2,   " +1 ",               FAIL);
     FUNCTION F IS NEW F1 (LIST,   " +1 ",               FAIL);
     FUNCTION F IS NEW F1 (WORD,   (' ', '+', '1', ' '), FAIL);
     FUNCTION F IS NEW F1 (STR,    (" ", "+", "1", " "), FAIL);
     FUNCTION F IS NEW F1 (LINE,   (' ', '+', '1', ' '), FAIL);
     FUNCTION F IS NEW F1 (MY_STRING, " +1 ",               PASS);
    
BEGIN
     TEST ("C87B07C","OVERLOADED OPERANDS TO THE 'VALUE' ATTRIBUTE");
    
     DECLARE
          TYPE INT IS NEW INTEGER;
          FUNCTION "-" (X : INT) RETURN INT 
               RENAMES "+";
 
     BEGIN
          IF INT'VALUE (F) /= -1 THEN
               FAILED ("THE ATTRIBUTE T'VALUE MUST RETURN A VALUE" &
                       " OF TYPE T");
          END IF;
     END;
 
     RESULT;
END C87B07C;
