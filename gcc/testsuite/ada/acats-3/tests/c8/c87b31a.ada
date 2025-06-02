-- C87B31A.ADA

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
-- IF THE TYPE OF AN AGGREGATE IS A ONE-DIMENSIONAL ARRAY TYPE
-- THEN EACH CHOICE MUST SPECIFY VALUES OF THE INDEX TYPE, AND
-- THE EXPRESSION OF EACH COMPONENT ASSOCIATION MUST BE OF THE
-- COMPONENT TYPE.
  
-- TRH  8 AUG 82
-- DSJ 15 JUN 83
-- JRK  2 FEB 84
-- JBG 4/23/84

WITH REPORT; USE REPORT;
   
PROCEDURE C87B31A IS
 
     TYPE LETTER  IS NEW CHARACTER RANGE 'A' .. 'Z';
     TYPE NOTE    IS (A, B, C, D, E, F, G, H);
     TYPE STR     IS NEW STRING (1 .. 1);
     TYPE BIT     IS NEW BOOLEAN;
     TYPE YES     IS NEW BOOLEAN RANGE TRUE  .. TRUE;
     TYPE NO      IS NEW BOOLEAN RANGE FALSE .. FALSE;
     TYPE BOOLEAN IS (FALSE, TRUE);
     TYPE LIST    IS ARRAY (CHARACTER RANGE <>) OF BIT;
     TYPE FLAG    IS (PASS,  FAIL);
     
     SUBTYPE LIST_A  IS LIST('A'..'A');
     SUBTYPE LIST_E  IS LIST('E'..'E');
     SUBTYPE LIST_AE IS LIST('A'..'E');

     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN 
          IF STAT = FAIL THEN 
               FAILED ("RESOLUTION INCORRECT FOR EXPRESSIONS " &
                       "IN ARRAY AGGREGATES");
          END IF;
          RETURN ARG;
     END F1;
  
     FUNCTION F IS NEW F1 (BOOLEAN, FALSE, FAIL);
     FUNCTION F IS NEW F1 (YES,     TRUE,  FAIL);
     FUNCTION F IS NEW F1 (NO,      FALSE, FAIL);
     FUNCTION F IS NEW F1 (BIT,     TRUE,  PASS);
  
     FUNCTION G IS NEW F1 (CHARACTER, 'A', PASS);
     FUNCTION G IS NEW F1 (LETTER,    'A', FAIL);
     FUNCTION G IS NEW F1 (STR,       "A", FAIL);
  
     FUNCTION H IS NEW F1 (CHARACTER, 'E', PASS);
     FUNCTION H IS NEW F1 (LETTER,    'E', FAIL);
     FUNCTION H IS NEW F1 (STR,       "E", FAIL);
    
BEGIN
     TEST ("C87B31A", "OVERLOADED EXPRESSIONS IN ARRAY AGGREGATES");
    
     DECLARE
          L1, L2 : LIST_A :=  (OTHERS => FALSE);
          L3, L4 : LIST_E :=  (OTHERS => FALSE);
          L5, L6 : LIST_AE := (OTHERS => FALSE);
          L7, L8 : LIST_AE := (OTHERS => FALSE);

     BEGIN
          L1 := ('A' => F);
          L2 := ( G  => F);
          L3 := ('E' => F);
          L4 := ( H  => F);
          L5 := ('A'..'E' => F);
          L6 := (F,F,F,F,F);
          L7 := (F,F,F, OTHERS => F);
          L8 := LIST_AE'('E' => F, 'B' => F, OTHERS => F);
  
          IF L1 /= LIST_A'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L1");
          END IF;
          IF L2 /= LIST_A'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L2");
          END IF;
          IF L3 /= LIST_E'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L3");
          END IF;
          IF L4 /= LIST_E'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L4");
          END IF;
          IF L5 /= LIST_AE'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L5");
          END IF;
          IF L6 /= LIST_AE'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L6");
          END IF;
          IF L7 /= LIST_AE'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L7");
          END IF;
          IF L8 /= LIST_AE'(OTHERS => TRUE) THEN
                 FAILED ("RESOLUTION INCORRECT FOR OVERLOADED" &
                         " EXPRESSIONS IN ARRAY AGGREGATES - L8");
          END IF;
     END;
 
     RESULT;
END C87B31A;
