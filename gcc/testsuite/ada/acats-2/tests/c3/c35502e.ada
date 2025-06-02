-- C35502E.ADA

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
-- CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT 
-- RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL 
-- PARAMETER IS AN ENUMERATION TYPE OTHER THAN A BOOLEAN OR A 
-- CHARACTER TYPE.   
-- SUBTESTS ARE:
--     PART (A). TESTS FOR IMAGE.
--     PART (B). TESTS FOR VALUE.

-- RJW 5/13/86

WITH REPORT; USE REPORT;

PROCEDURE  C35502E  IS

          TYPE ENUM IS (A, BC, ABC, A_B_C, abcd);
          SUBTYPE SUBENUM IS ENUM RANGE A .. BC;

          TYPE NEWENUM IS NEW ENUM;

BEGIN

     TEST( "C35502E" , "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
                       "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
                       "ACTUAL PARAMETER IS AN ENUMERATION TYPE " &
                       "OTHER THAN A BOOLEAN OR A CHARACTER TYPE" );

-- PART (A).
     DECLARE
          GENERIC
               TYPE E IS (<>);
               STR1 : STRING;
          PROCEDURE P ( E1 : E; STR2 : STRING );
          
          PROCEDURE P ( E1 : E; STR2 : STRING ) IS
               SUBTYPE SE IS E RANGE E'VAL(0) .. E'VAL(1);
          BEGIN
               IF SE'IMAGE ( E1 ) /= STR2 THEN
                    FAILED ( "INCORRECT SE'IMAGE FOR " & STR2 & " IN "
                              & STR1 );
               END IF;
               IF SE'IMAGE ( E1 )'FIRST /= 1 THEN
                    FAILED ( "INCORRECT LOWER BOUND FOR " & STR2 
                              & " IN " & STR1 );
               END IF;
          END P;
          
          PROCEDURE PE IS NEW P ( ENUM , "ENUM" );
          PROCEDURE PS IS NEW P ( SUBENUM, "SUBENUM" );
          PROCEDURE PN IS NEW P ( NEWENUM, "NEWENUM" );

     BEGIN
          PE ( ABC, "ABC" );
          PE ( A_B_C, "A_B_C" );
          PS ( BC, "BC" );
          PN ( ABC, "ABC" );
          PE ( abcd, "ABCD" );
     END;

-----------------------------------------------------------------------

-- PART (B).

     DECLARE
          GENERIC
               TYPE E IS (<>);
               STR1 : STRING;
          PROCEDURE P ( STR2 : STRING ; E1 : E );
          
          PROCEDURE P ( STR2 : STRING ; E1 : E ) IS
               SUBTYPE SE IS E RANGE E'VAL(0) .. E'VAL(1);
          BEGIN
               IF E'VALUE ( STR2 ) /= E1 THEN     
                    FAILED ( "INCORRECT " & STR1 & "'VALUE FOR """ & 
                              STR2 & """" ); 
               END IF;
          EXCEPTION     
               WHEN OTHERS =>
                    FAILED ( "EXCEPTION RAISED - " & STR1 & "'VALUE " &
                             "FOR """ & STR2 & """" );  
          END P;

          PROCEDURE PE IS NEW P ( ENUM , "ENUM" );
          PROCEDURE PN IS NEW P ( NEWENUM, "NEWENUM" );

     BEGIN
          PN ("abcd", abcd);
          PN ("A_B_C", A_B_C);
          PE ("ABC     ", ABC);
          PE ("  A_B_C", A_B_C);
     END;


     DECLARE
          GENERIC
               TYPE E IS (<>);
          PROCEDURE P ( STR : STRING );
          
          PROCEDURE P ( STR : STRING ) IS
               SUBTYPE SE IS E RANGE E'VAL(0) .. E'VAL(1);
          BEGIN
               IF SE'VALUE (STR) = SE'VAL (0) THEN
                    FAILED ( "NO EXCEPTION RAISED - " & STR & " - 1" );
               ELSE
                    FAILED ( "NO EXCEPTION RAISED - " & STR & " - 2" );
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED - " & STR );
          END P;

          PROCEDURE PE IS NEW P ( ENUM );
          PROCEDURE PS IS NEW P ( SUBENUM );
          PROCEDURE PN IS NEW P ( NEWENUM );
          
     BEGIN
          PS ("A BC");
          PN ("A&BC");
          PE (ASCII.HT & "BC");
          PE ("A" & ASCII.HT);
          PS ("_BC");
          PN ("BC_");
          PE ("B__C");
          PE ("0BC");

     END;

     RESULT;
END C35502E;
