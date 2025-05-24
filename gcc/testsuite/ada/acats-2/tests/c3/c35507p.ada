-- C35507P.ADA

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
-- CHECK THAT THE ATTRIBUTES 'FIRST' AND 'LAST' YIELD THE CORRECT 
-- RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL
-- PARAMETER IS A CHARACTER TYPE.   

-- RJW 6/03/86
-- PWN 11/30/94 REMOVED TESTS BASED ON 128 CHARACTERS FOR ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE  C35507P  IS

     TYPE CHAR IS ('A', B);

     TYPE NEWCHAR IS NEW CHAR;     

     SPACE : CONSTANT CHARACTER := ' ';

     SUBTYPE GRAPHIC IS CHARACTER RANGE SPACE .. ASCII.TILDE;
     SUBTYPE NONGRAPHIC IS CHARACTER RANGE ASCII.NUL .. ASCII.US;
BEGIN

     TEST( "C35507P" , "CHECK THAT THE ATTRIBUTES 'FIRST' AND " &
                       "'LAST' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
                       "ACTUAL PARAMETER IS A CHARACTER TYPE" );

     DECLARE
          GENERIC
               TYPE CHTYPE IS (<>);
               STR : STRING;
               F, L : CHTYPE;
          PROCEDURE P;

          PROCEDURE P IS 
               SUBTYPE NOCHAR IS CHTYPE RANGE L .. F;
          BEGIN 
               IF CHTYPE'FIRST /= F THEN 
                    FAILED ( "INCORRECT VALUE FOR " & STR & "'FIRST" );
               END IF;

               IF CHTYPE'LAST /= L THEN 
                    FAILED ( "INCORRECT VALUE FOR " & STR & "'LAST" );
               END IF;

               IF NOCHAR'FIRST /= L THEN 
                    FAILED ( "INCORRECT VALUE FOR NOCHAR'FIRST AS A " &
                             "SUBTYPE OF " & STR );
               END IF;

               IF NOCHAR'LAST /= F THEN 
                    FAILED ( "INCORRECT VALUE FOR NOCHAR'LAST AS A " &
                             "SUBTYPE OF " & STR  );
               END IF;
          END P;
          
          PROCEDURE P1 IS NEW P (CHAR, "CHAR", 'A', B);          
          PROCEDURE P2 IS NEW P (NEWCHAR, "NEWCHAR", 'A', B);          
          PROCEDURE P3 IS NEW P 
               (GRAPHIC, "GRAPHIC", SPACE, ASCII.TILDE);
          PROCEDURE P4 IS NEW P 
               (NONGRAPHIC, "NONGRAPHIC", ASCII.NUL, ASCII.US);
     BEGIN
          P1;
          P2;
          P3;
          P4;
     END;

     RESULT;
END C35507P;
