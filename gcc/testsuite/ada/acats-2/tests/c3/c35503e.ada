-- C35503E.ADA

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
--     CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE CORRECT RESULTS WHEN
--     THE PREFIX IS A GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL
--     PARAMETER IS AN INTEGER TYPE.
--     SUBTESTS ARE :
--         PART (A). TESTS FOR 'IMAGE'.
--         PART (B). TESTS FOR 'VALUE'.

-- HISTORY:
--     RJW 03/17/86 CREATED ORIGINAL TEST.
--     DHH 10/19/87 SHORTENED LINES CONTAINING MORE THAN 72 CHARACTERS.

WITH REPORT; USE REPORT;

PROCEDURE C35503E IS

BEGIN
     TEST ("C35503E", "CHECK THAT 'IMAGE' AND 'VALUE' YIELD THE " &
                      "CORRECT RESULTS  WHEN THE PREFIX IS A " &
                      "GENERIC FORMAL DISCRETE TYPE WHOSE ACTUAL " &
                      "PARAMETER IS AN INTEGER TYPE" );
-- PART (A).

     DECLARE
          TYPE NEWINT IS NEW INTEGER RANGE -2000 .. 2000;

          GENERIC
               TYPE INT IS (<>);
          PROCEDURE P (I1 : INT; STR : STRING );

          PROCEDURE P (I1 : INT; STR : STRING) IS
               SUBTYPE SUBINT IS INT
               RANGE INT'VAL (IDENT_INT(-1000)) ..
                                              INT'VAL (IDENT_INT(1000));
          BEGIN

               IF INT'IMAGE (I1) /= STR THEN
                    FAILED ( "INCORRECT INT'IMAGE OF " & STR );
               END IF;
               IF INT'IMAGE (I1)'FIRST /= 1 THEN
                    FAILED ( "INCORRECT LOWER BOUND FOR INT'IMAGE OF " &
                             STR );
               END IF;

               IF SUBINT'IMAGE (I1) /= STR THEN
                    FAILED ( "INCORRECT SUBINT'IMAGE OF " & STR );
               END IF;
               IF SUBINT'IMAGE (I1)'FIRST /= 1 THEN
                    FAILED ( "INCORRECT LOWER BOUND FOR SUBINT'IMAGE " &
                             "OF " & STR );
               END IF;

          END P;

          PROCEDURE PROC1 IS NEW P (INTEGER);
          PROCEDURE PROC2 IS NEW P (NEWINT);

     BEGIN
          PROC1 (-500, "-500");
          PROC2 (0, " 0");
          PROC2 (99," 99");
     END;

-----------------------------------------------------------------------

-- PART (B).

     DECLARE
          TYPE NEWINT IS NEW INTEGER;

          GENERIC
               TYPE INT IS (<>);
          PROCEDURE P (STR : STRING; I1 : INT );

          PROCEDURE P (STR : STRING; I1 : INT) IS
               SUBTYPE SUBINT IS INT
                    RANGE INT'VAL (IDENT_INT(0)) ..
                                                INT'VAL (IDENT_INT(10));

          BEGIN
               BEGIN
                    IF INT'VALUE (STR) /= I1 THEN
                         FAILED ( "INCORRECT INT'VALUE OF """ &
                                  STR & """");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED INT'VALUE OF """ &
                                  STR & """");
               END;
               BEGIN
                    IF SUBINT'VALUE (STR) /= I1 THEN
                         FAILED ( "INCORRECT SUBINT'VALUE OF """ &
                                  STR & """");
                    END IF;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ( "EXCEPTION RAISED SUBINT'VALUE " &
                                  "OF """ & STR & """");
               END;
          END P;

          PROCEDURE PROC1 IS NEW P (INTEGER);
          PROCEDURE PROC2 IS NEW P (NEWINT);

     BEGIN
               PROC1 ("-500" , -500);
               PROC2 (" -001E2 " , -100);
               PROC1 ("3_45" , 345);
               PROC2 ("-2#1111_1111#" , -255);
               PROC1 ("16#FF#" , 255);
               PROC2 ("-016#0FF#" , -255);
               PROC1 ("2#1110_0000#     " , 224);
               PROC2 ("-16#E#E1" , -224);

     END;

     DECLARE
          TYPE NEWINT IS NEW INTEGER;

          GENERIC
               TYPE INT IS (<>);
          PROCEDURE P (STR1 : STRING; I1 : INT; STR2 : STRING);

          PROCEDURE P (STR1 : STRING; I1 : INT; STR2 : STRING) IS
               SUBTYPE SUBINT IS INT
                    RANGE INT'VAL (IDENT_INT(0)) ..
                                                INT'VAL (IDENT_INT(10));

          BEGIN
               BEGIN
                    IF INT'VALUE (STR1) = I1 THEN
                         FAILED ( "NO EXCEPTION RAISED - INT'VALUE " &
                                  "WITH " & STR2 & " - EQUAL");
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED " &
                                  "- INT'VALUE WITH " &
                                  STR2 & " - NOT EQUAL" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED - " &
                                  "INT'VALUE WITH " & STR2 );
               END;
               BEGIN
                    IF SUBINT'VALUE (STR1) = I1 THEN
                         FAILED ( "NO EXCEPTION RAISED - " &
                                  "SUBINT'VALUE WITH " & STR2
                                   & " - EQUAL" );
                    ELSE
                         FAILED ( "NO EXCEPTION RAISED - " &
                                  "SUBINT'VALUE WITH " &
                                   STR2 & " - NOT EQUAL" );
                    END IF;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ( "WRONG EXCEPTION RAISED  - " &
                                  "SUBINT'VALUE WITH " & STR2 );
               END;
          END P;

          PROCEDURE PROC1 IS NEW P (INTEGER);
          PROCEDURE PROC2 IS NEW P (NEWINT);

     BEGIN
          PROC1 ("1.0"           , 1,      "DECIMAL POINT");
          PROC1 (ASCII.HT & "244", 244,    "LEADING 'HT'" );
          PROC2 ("244" & ASCII.HT, 244,    "TRAILING 'HT'" );
          PROC1 ("2__44"         , 244,    "CONSECUTIVE '_'" );
          PROC2 ("_244"          , 244,    "LEADING '_'" );
          PROC1 ("244_"          , 244,    "TRAILING '_'" );
          PROC2 ("244_E1"        , 2440,   "'_' BEFORE 'E'" );
          PROC1 ("244E_1"        , 2440,   "'_' FOLLOWING 'E'" );
          PROC2 ("244_e1"        , 2440,   "'_' BEFORE 'e'" );
          PROC1 ("16#_FF#"       , 255,    "'_' IN BASED LITERAL" );
          PROC2 ("1E-0"          ,   0,    "NEGATIVE EXPONENT" );
          PROC1 ("244."          , 244,    "TRAILING '.'" );
          PROC2 ("8#811#"        , 0,      "DIGITS OUTSIDE OF RANGE" );
          PROC1 ("1#000#"        , 0,      "BASE LESS THAN 2" );
          PROC2 ("17#0#"         , 0,      "BASE GREATER THAN 16" );
     END;

     RESULT;
END C35503E;
