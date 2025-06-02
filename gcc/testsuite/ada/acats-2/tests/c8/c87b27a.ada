-- C87B27A.ADA

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
-- THE TYPE OF A STRING LITERAL MUST BE DETERMINED FROM THE FACT
-- THAT A STRING LITERAL IS A VALUE OF A ONE DIMENSIONAL ARRAY OF
-- CHARACTER COMPONENTS.
  
-- TRH  18 AUG 82
-- DSJ  07 JUN 83
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B27A IS
 
     TYPE ENUMLIT   IS (A, B, C, D, E, F);
     TYPE NEW_CHAR  IS NEW CHARACTER RANGE 'G' .. 'Z';
     TYPE CHARS3    IS ('G','H','I','K','M','N','P','R','S','T');
     TYPE CHARS4    IS ('S','T','R','I','N','G','Z','A','P');
     TYPE NEW_STR   IS ARRAY  (A .. F) OF NEW_CHAR;
     TYPE STRING3   IS ARRAY  (11..16) OF CHARS3;
     TYPE STRING4   IS ARRAY  (21..26) OF CHARS4;
     TYPE ENUM_VEC  IS ARRAY  (1 .. 6) OF ENUMLIT;
     TYPE CHAR_GRID IS ARRAY  (D .. F, 1 .. 3) OF NEW_CHAR;
     TYPE STR_LIST  IS ARRAY  (1 .. 6) OF STRING (1 .. 1);
     ERR : BOOLEAN  := FALSE;
 
     PROCEDURE P (X : NEW_STR) IS
     BEGIN
          NULL;
     END P;
 
     PROCEDURE P (X : ENUM_VEC) IS
     BEGIN
          ERR := TRUE;
     END P;
 
     PROCEDURE P (X : CHAR_GRID) IS
     BEGIN
          ERR := TRUE;
     END P;
 
     PROCEDURE P (X : STR_LIST) IS
     BEGIN
          ERR := TRUE;
     END P;
 
BEGIN
     TEST ("C87B27A","OVERLOADING RESOLUTION OF STRING LITERALS");
    
     P ("STRING");
 
     IF ERR THEN
          FAILED ("RESOLUTION INCORRECT FOR STRING LITERALS");
     END IF;
 
     RESULT;
END C87B27A;
