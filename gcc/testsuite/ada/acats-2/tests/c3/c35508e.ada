-- C35508E.ADA

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
--     CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE
--     ACTUAL ARGUMENT IS A BOOLEAN TYPE.

--     SUBTESTS ARE:
--         (A). TESTS FOR IMAGE.
--         (B). TESTS FOR VALUE.

-- HISTORY:
--     RJW 03/19/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE  C35508E  IS

BEGIN

     TEST( "C35508E" , "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
                       "'VALUE' YIELD THE CORRECT RESULTS  WHEN THE " &
                       "PREFIX IS A GENERIC FORMAL DISCRETE TYPE " &
                       "WHOSE ACTUAL ARGUMENT IS A BOOLEAN TYPE" );
-- PART (A).

     DECLARE
          TYPE NEWBOOL IS NEW BOOLEAN;

          GENERIC
               TYPE BOOL IS (<>);
          PROCEDURE P (B : BOOL; STR : STRING );

          PROCEDURE P (B : BOOL; STR : STRING) IS
               SUBTYPE SUBBOOL IS BOOL
                    RANGE BOOL'VAL (IDENT_INT(0)) ..
                                               BOOL'VAL (IDENT_INT(0));
          BEGIN

               IF BOOL'IMAGE (B) /= STR THEN
                    FAILED ( "INCORRECT BOOL'IMAGE OF " & STR );
               END IF;
               IF BOOL'IMAGE (B)'FIRST /= 1 THEN
                    FAILED ( "INCORRECT BOOL'FIRST FOR " & STR );
               END IF;

               IF SUBBOOL'IMAGE (B) /= STR THEN
                    FAILED ( "INCORRECT SUBBOOL'IMAGE OF " & STR );
               END IF;
               IF SUBBOOL'IMAGE (B)'FIRST /= 1 THEN
                    FAILED ( "INCORRECT SUBBOOL'FIRST FOR " & STR );
               END IF;
          END P;

          PROCEDURE NP1 IS NEW P ( BOOLEAN );
          PROCEDURE NP2 IS NEW P ( NEWBOOL );
     BEGIN
          NP1 ( TRUE, "TRUE" );
          NP2 ( FALSE, "FALSE" );

     END;

-----------------------------------------------------------------------

-- PART (B).

     DECLARE
          TYPE NEWBOOL IS NEW BOOLEAN;

          GENERIC
               TYPE BOOL IS (<>);
          PROCEDURE P (STR : STRING; B : BOOL );

          PROCEDURE P (STR : STRING; B : BOOL) IS
               SUBTYPE SUBBOOL IS BOOL
                    RANGE BOOL'VAL (IDENT_INT(0)) ..
                                               BOOL'VAL (IDENT_INT(0));

          BEGIN
               BEGIN
                    IF BOOL'VALUE (STR) /= B THEN
                         FAILED ( "INCORRECT BOOL'VALUE OF """ &
                                  STR & """" );
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED BOOL'VALUE OF """ &
                                  STR & """" );
               END;
               BEGIN
                    IF SUBBOOL'VALUE (STR) /= B THEN
                         FAILED ( "INCORRECT SUBBOOL'VALUE OF """ &
                                  STR & """" );
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED SUBBOOL'VALUE " &
                                  "OF """ & STR & """" );
               END;
          END P;

          PROCEDURE NP1 IS NEW P ( BOOLEAN );
          PROCEDURE NP2 IS NEW P ( NEWBOOL );

     BEGIN
          NP1 ( "TRUE", TRUE );
          NP2 ( "FALSE", FALSE );
          NP2 ( "true", TRUE );
          NP1 ( "false", FALSE );
          NP1 ( "         TRUE", TRUE );
          NP2 ( "FALSE        ", FALSE );
     END;

     DECLARE
          GENERIC
               TYPE BOOL IS (<>);
          PROCEDURE P (STR1 : STRING; B : BOOL; STR2 : STRING);

          PROCEDURE P (STR1 : STRING; B : BOOL; STR2 : STRING) IS
               SUBTYPE SUBBOOL IS BOOL
                    RANGE BOOL'VAL (IDENT_INT(0)) ..
                                                BOOL'VAL (IDENT_INT(0));

          BEGIN
               BEGIN
                    IF BOOL'VALUE (STR1) = B THEN
                         FAILED ( "NO EXCEPTION RAISED - " &
                                  "BOOL'VALUE WITH " & STR2 &
                                  "- EQUAL " );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED - " &
                                  "BOOL'VALUE WITH " & STR2 &
                                  " - NOT EQUAL" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED - " &
                                  "BOOL'VALUE WITH " & STR2 );
               END;
               BEGIN
                    IF SUBBOOL'VALUE (STR1) /= B THEN
                         FAILED ( "NO EXCEPTION RAISED - " &
                                  "SUBBOOL'VALUE WITH " &
                                  STR2 & " - EQUAL");
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED - " &
                                  "SUBBOOL'VALUE WITH " &
                                  STR2 & " - NOT EQUAL");
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED - " &
                                  "SUBBOOL'VALUE WITH " & STR2 );
               END;
          END P;

          PROCEDURE NP IS NEW P ( BOOLEAN );
     BEGIN
          NP ( "MAYBE", TRUE, "NON-BOOLEAN VALUE");
          NP ( ASCII.HT & "TRUE", TRUE, "LEADING 'HT'" );
          NP ( "FALSE" & ASCII.HT , FALSE, "TRAILING 'HT'" );
     END;

     RESULT;
END C35508E;
