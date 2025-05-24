-- C87B19A.ADA

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
-- OBJECTIVE:
--     CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
--     SIMPLE EXPRESSIONS AND RANGE BOUNDS OF VARIANT CHOICES MUST MATCH
--     THE TYPE OF THE DISCRIMINANT'S EXPLICIT TYPEMARK.

--HISTORY:
--     DSJ 06/15/83 CREATED ORIGINAL TEST.
--     DHH 10/20/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C87B19A IS

     TYPE COLOR  IS (YELLOW, RED, BLUE, GREEN, BROWN);
     TYPE SCHOOL IS (YALE, HARVARD, PRINCETON, BROWN, STANFORD);
     TYPE COOK   IS (BROIL, BAKE, BROWN, TOAST, FRY);
     TYPE MIXED  IS (GREEN, BROWN, YALE, BAKE, BLUE, FRY);

     RATING : INTEGER := 0;

     FUNCTION OK RETURN BOOLEAN IS
     BEGIN
          RATING := RATING + 1;
          RETURN FALSE;
     END OK;

     FUNCTION ERR RETURN BOOLEAN IS
     BEGIN
          FAILED ("VARIANT CHOICES MUST MATCH TYPE OF DISCRIMINANT");
          RETURN FALSE;
     END ERR;

BEGIN
     TEST ("C87B19A","OVERLOADED EXPRESSIONS AND RANGE BOUNDS" &
           " OF VARIANT CHOICES");
     DECLARE

          TYPE REC (X : MIXED := BROWN) IS
               RECORD
                    CASE X IS
                         WHEN GREEN .. BROWN  => NULL;
                         WHEN BLUE            => NULL;
                         WHEN FRY             => NULL;
                         WHEN YALE            => NULL;
                         WHEN OTHERS          => NULL;
                    END CASE;
               END RECORD;

          R1 : REC (X => FRY);
          R2 : REC (X => BLUE);
          R3 : REC (X => BAKE);
          R4 : REC (X => YALE);
          R5 : REC (X => BROWN);
          R6 : REC (X => GREEN);

     BEGIN
          IF MIXED'POS(R1.X) /= 5 THEN
             FAILED ("VARIANT CHOICES MUST MATCH TYPE OF " &
                     "DISCRIMINANT-R1");
          END IF;
          IF MIXED'POS(R2.X) /= 4 THEN
             FAILED ("VARIANT CHOICES MUST MATCH TYPE OF " &
                     "DISCRIMINANT-R2");
          END IF;
          IF MIXED'POS(R3.X) /= 3 THEN
             FAILED ("VARIANT CHOICES MUST MATCH TYPE OF " &
                     "DISCRIMINANT-R3");
          END IF;
          IF MIXED'POS(R4.X) /= 2 THEN
             FAILED ("VARIANT CHOICES MUST MATCH TYPE OF " &
                     "DISCRIMINANT-R4");
          END IF;
          IF MIXED'POS(R5.X) /= 1 THEN
             FAILED ("VARIANT CHOICES MUST MATCH TYPE OF " &
                     "DISCRIMINANT-R5");
          END IF;
          IF MIXED'POS(R6.X) /= 0 THEN
             FAILED ("VARIANT CHOICES MUST MATCH TYPE OF " &
                     "DISCRIMINANT-R6");
          END IF;

     END;

     RESULT;
END C87B19A;
