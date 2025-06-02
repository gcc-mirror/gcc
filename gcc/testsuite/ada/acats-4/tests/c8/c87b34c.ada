-- C87B34C.ADA

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
    
-- FOR A MEMBERSHIP RELATION WITH A TYPEMARK, THE TYPE OF THE
-- SIMPLE EXPRESSION MUST BE THE BASE TYPE OF THE TYPEMARK.
  
-- TRH  15 SEPT 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B34C IS
 
     TYPE VOWEL IS (A, E, I, O, U, VOCALIC_Y);
     TYPE ALPHA IS (A, 'A');
     TYPE GRADE IS (A, B, C, D, F);
     SUBTYPE BAD_GRADE IS GRADE RANGE D .. F;
     SUBTYPE PASSING   IS GRADE RANGE A .. C;
 
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          FAILED ("RESOLUTION INCORRECT - EXPRESSION IN MEMBER" &
                  "SHIP TEST WITH TYPEMARK MUST MATCH TYPEMARK");
          RETURN ARG;
     END F1;
 
     FUNCTION F IS NEW F1 (CHARACTER, 'A');
     FUNCTION F IS NEW F1 (DURATION,  1.0);
     FUNCTION F IS NEW F1 (INTEGER,   -10);
     FUNCTION F IS NEW F1 (BOOLEAN,   TRUE);
     FUNCTION F IS NEW F1 (FLOAT,     1.0);
     FUNCTION F IS NEW F1 (VOWEL,     A);
     FUNCTION F IS NEW F1 (ALPHA,     A);
    
BEGIN
     TEST ("C87B34C","OVERLOADED EXPRESSION IN MEMBERSHIP TEST " &
           "WITH A TYPEMARK");
 
     IF (F NOT IN GRADE) OR (F NOT IN BAD_GRADE)
          OR (F IN PASSING) THEN 
          FAILED ("RESOLUTION INCORRECT FOR MEMBERSHIP TEST " &
                  "WITH TYPEMARK");
     END IF;
    
     RESULT;
 
END C87B34C;
