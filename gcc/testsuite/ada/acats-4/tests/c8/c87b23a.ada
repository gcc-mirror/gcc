-- C87B23A.ADA

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
-- FOR AN INDEXED COMPONENT OF AN ARRAY, THE PREFIX MUST BE 
-- APPROPRIATE FOR AN ARRAY TYPE. EACH EXPRESSION FOR THE INDEXED 
-- COMPONENT MUST BE OF THE TYPE OF THE CORRESPONDING INDEX AND
-- THERE MUST BE ONE SUCH EXPRESSION FOR EACH INDEX POSITION OF THE 
-- ARRAY TYPE.
  
-- TRH  15 SEPT 82
-- DSJ  07 JUNE 83
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B23A IS

     SUBTYPE CHAR IS CHARACTER;
     TYPE GRADE IS (A, B, C, D, F);
     TYPE NOTE  IS (A, B, C, D, E, F, G);
     TYPE INT   IS NEW INTEGER;
     TYPE POS   IS NEW INTEGER RANGE 1 .. INTEGER'LAST;
     TYPE NAT   IS NEW POS;
     TYPE BOOL  IS NEW BOOLEAN;
     TYPE BIT   IS NEW BOOL;
     TYPE LIT   IS (FALSE, TRUE);
     TYPE FLAG  IS (PASS, FAIL);

     TYPE NUM2 IS DIGITS(2);
     TYPE NUM3 IS DIGITS(2);
     TYPE NUM4 IS DIGITS(2);

     TYPE A1 IS ARRAY (POS'(1)..5, NOTE'(A)..D, BOOL'(FALSE)..TRUE)
          OF FLOAT;
     TYPE A2 IS ARRAY (INT'(1)..5, NOTE'(A)..D, BIT'(FALSE)..TRUE)
          OF NUM2;
     TYPE A3 IS ARRAY (POS'(1)..5, GRADE'(A)..D, BOOL'(FALSE)..TRUE)
          OF NUM3;
     TYPE A4 IS ARRAY (NAT'(1)..5, NOTE'(A)..D, LIT'(FALSE)..TRUE)
          OF NUM4;
     
     OBJ1 : A1 := (OTHERS => (OTHERS => (OTHERS => 0.0)));
     OBJ2 : A2 := (OTHERS => (OTHERS => (OTHERS => 0.0)));
     OBJ3 : A3 := (OTHERS => (OTHERS => (OTHERS => 0.0)));
     OBJ4 : A4 := (OTHERS => (OTHERS => (OTHERS => 0.0)));

     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
               FAILED ("PREFIX OR INDEX IS NOT APPROPRIATE FOR" &
                       " INDEXED COMPONENT");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION A IS NEW F1 (A1, OBJ1, PASS);
     FUNCTION A IS NEW F1 (A2, OBJ2, FAIL);
     FUNCTION A IS NEW F1 (A3, OBJ3, FAIL);
     FUNCTION A IS NEW F1 (A4, OBJ4, FAIL);
  
BEGIN
     TEST ("C87B23A","OVERLOADED ARRAY INDEXES");
    
     DECLARE
          F1 : FLOAT := A (3, C, TRUE);
   
     BEGIN
          NULL;
     END;
 
     RESULT;
END C87B23A;
