-- C35507E.ADA

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
--     CHECK THAT THE ATTRIBUTES 'IMAGE' AND 'VALUE YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A FORMAL DISCRETE TYPE WHOSE ACTUAL
--     PARAMETER IS A CHARACTER TYPE.
--     SUBTESTS ARE:
--         (A). TESTS FOR IMAGE.
--         (B). TESTS FOR VALUE.

-- HISTORY:
--     RJW  05/29/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER, CHANGED THE CALLS TO
--                    PROCEDURE 'PCH', IN THE SECOND PART OF SUBTEST B,
--                    TO INCLUDE ANOTHER CALL TO PROCEDURE 'PCHAR' AND
--                    CALLS TO PROCEDURE 'PNCHAR'.

WITH REPORT; USE REPORT;
PROCEDURE  C35507E  IS

     TYPE CHAR IS ('A', 'a');

     TYPE NEWCHAR IS NEW CHAR;

     PROCEDURE CHECK_LOWER_BOUND (STR1, STR2 : STRING) IS
     BEGIN
          IF STR1'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR " & STR2 & "'(" &
                        STR1 & ")" );
          END IF;
     END CHECK_LOWER_BOUND;

BEGIN

     TEST( "C35507E" , "THE ATTRIBUTES 'IMAGE' AND " &
                       "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A FORMAL DISCRETE TYPE WHOSE " &
                       "ACTUAL PARAMETER IS A CHARACTER TYPE" );

     DECLARE -- (A).
          GENERIC
               TYPE CHTYPE IS (<>);
               STR1 : STRING;
          PROCEDURE P (CH : CHTYPE; STR2 : STRING);

          PROCEDURE P (CH : CHTYPE; STR2 : STRING) IS
               SUBTYPE SUBCH IS CHTYPE;
          BEGIN
               IF SUBCH'IMAGE (CH) /= STR2 THEN
                    FAILED ( "INCORRECT IMAGE FOR " & STR1 & "'(" &
                              STR2 & ")" );
               END IF;

               CHECK_LOWER_BOUND (SUBCH'IMAGE (CH), STR1);
          END P;

          PROCEDURE PCHAR  IS NEW P (CHAR, "CHAR");
          PROCEDURE PNCHAR IS NEW P (NEWCHAR, "NEWCHAR");
          PROCEDURE PCH    IS NEW P (CHARACTER, "CHARACTER");

     BEGIN
          PCHAR ('A', "'A'");
          PCHAR ('a', "'a'");
          PNCHAR ('A', "'A'");
          PNCHAR ('a', "'a'");

          FOR CH IN CHARACTER'VAL (32) .. CHARACTER'VAL (126) LOOP
               PCH (CH, ("'" & CH) & "'" );
          END LOOP;
     END;

     DECLARE

          GENERIC
               TYPE CHTYPE IS (<>);
          PROCEDURE P (CH : CHTYPE; STR : STRING);

          PROCEDURE P (CH : CHTYPE; STR : STRING) IS
               SUBTYPE SUBCH IS CHTYPE;
          BEGIN
               CHECK_LOWER_BOUND (CHTYPE'IMAGE (CH), "CHARACTER");
          END P;

          PROCEDURE PN IS NEW P (CHARACTER);

     BEGIN

          FOR CH IN CHARACTER'VAL (0) .. CHARACTER'VAL (31) LOOP
               PN (CH, CHARACTER'IMAGE (CH));
          END LOOP;

          PN (ASCII.DEL, CHARACTER'IMAGE (ASCII.DEL));
     END;

     ---------------------------------------------------------------

     DECLARE -- (B).

          GENERIC
               TYPE CHTYPE IS (<>);
               STR1 : STRING;
          PROCEDURE P (STR2 : STRING; CH : CHTYPE);

          PROCEDURE P (STR2 : STRING; CH : CHTYPE) IS
               SUBTYPE SUBCH IS CHTYPE;
          BEGIN
               IF SUBCH'VALUE (STR2) /= CH THEN
                    FAILED ( "INCORRECT " & STR1 & "'VALUE FOR " &
                              STR2 );
               END IF;
          END P;

          PROCEDURE PCH IS NEW P (CHARACTER, "CHARACTER");
          PROCEDURE PCHAR IS NEW P (CHAR, "CHAR");
          PROCEDURE PNCHAR IS NEW P (NEWCHAR, "NEWCHAR");

     BEGIN
          FOR CH IN CHARACTER'VAL (0) .. CHARACTER'VAL (31) LOOP
                PCH (CHARACTER'IMAGE (CH), CH );
          END LOOP;

          PCH (CHARACTER'IMAGE (CHARACTER'VAL (127)),
               CHARACTER'VAL (127));

          PCHAR ("'A'", 'A');
          PCHAR ("'a'", 'a' );
          PNCHAR ("'A'", 'A');
          PNCHAR ("'a'", 'a');
     END;

     DECLARE
          GENERIC
               TYPE CHTYPE IS (<>);
               STR1 : STRING;
          PROCEDURE P (STR2 : STRING);

          PROCEDURE P (STR2 : STRING) IS
               SUBTYPE SUBCH IS CHTYPE;
          BEGIN
               IF SUBCH'VALUE (STR2) = SUBCH'VAL (0) THEN
                    FAILED ( "NO EXCEPTION RAISED FOR " &
                              STR1 & "'VALUE (" & STR2 & ") - 1" );
               ELSE
                    FAILED ( "NO EXCEPTION RAISED FOR " &
                              STR1 & "'VALUE (" & STR2 & ") - 2" );
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ( "WRONG EXCEPTION RAISED " &
                             "FOR " & STR1 & "'VALUE (" & STR2 & ")" );
          END P;

          PROCEDURE PCH IS NEW P (CHARACTER, "CHARACTER");
          PROCEDURE PCHAR IS NEW P (CHAR, "CHAR");
          PROCEDURE PNCHAR IS NEW P (NEWCHAR, "NEWCHAR");

     BEGIN
          PCHAR ("'B'");
          PCH (ASCII.HT & "'A'");
          PCH ("'B'" & ASCII.HT);
          PCH ("'C'" & ASCII.BEL);
          PCH ("'");
          PNCHAR ("''");
          PCHAR ("'A");
          PNCHAR ("A'");
          PCH ("'AB'");
     END;

     RESULT;
END C35507E;
