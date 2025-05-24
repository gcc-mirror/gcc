-- C35507L.ADA

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
-- CHECK THAT THE ATTRIBUTES 'POS' AND 'VAL' YIELD THE CORRECT 
-- RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL
-- PARAMETER IS A CHARACTER TYPE.   

-- RJW 6/03/86
-- PWN 11/30/94 REMOVED TESTS BASED ON 128 CHARACTERS FOR ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE  C35507L  IS

     TYPE CHAR IS ('A', B);

     TYPE NEWCHAR IS NEW CHAR;     

BEGIN

     TEST( "C35507L" , "CHECK THAT THE ATTRIBUTES 'POS' AND " &
                       "'VAL' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " & 
                       "ACTUAL PARAMETER IS A CHARACTER TYPE" );

     DECLARE
          GENERIC
               TYPE CHTYPE IS (<>);
               STR : STRING;
               I1 : INTEGER;
          PROCEDURE P;

          PROCEDURE P IS
               SUBTYPE SUBCH IS CHTYPE;               
               CH : CHTYPE;
               POSITION : INTEGER;
          BEGIN
               POSITION := 0;
               FOR CH IN CHTYPE LOOP
                    IF SUBCH'POS (CH) /= POSITION THEN 
                         FAILED ( "INCORRECT VALUE FOR " & STR & 
                                  "'POS OF " & CHTYPE'IMAGE (CH) );
                    END IF;
     
                    IF SUBCH'VAL (POSITION) /= CH THEN
                         FAILED ( "INCORRECT VALUE FOR " & STR & 
                                  "'VAL OF CHARACTER IN POSITION - " &
                                    INTEGER'IMAGE (POSITION) );
                    END IF;
                    POSITION := POSITION + 1;
               END LOOP;
     
               BEGIN
                    IF SUBCH'VAL (-1) = SUBCH'VAL (0) THEN
                         FAILED ( "NO EXCEPTION RAISED " & 
                                  "FOR " & STR & "'VAL (-1) - 1" );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED " & 
                                  "FOR " & STR & "'VAL (-1) - 2" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED " & 
                                  "FOR " & STR & "'VAL (-1)" );
               END;     
          END P;
          
          PROCEDURE PCHAR IS NEW P (CHAR, "CHAR", 1);
          PROCEDURE PNCHAR IS NEW P (NEWCHAR, "NEWCHAR", 1);
          PROCEDURE PCH IS NEW P (CHARACTER, "CHARACTER", 127);
     BEGIN
          PCHAR;
          PNCHAR;
          PCH;
     END;

     RESULT;
END C35507L;
