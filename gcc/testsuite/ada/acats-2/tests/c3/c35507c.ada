-- C35507C.ADA

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
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.
--     SUBTESTS ARE:
--         (A). TESTS FOR IMAGE.
--         (B). TESTS FOR VALUE.

-- HISTORY:
--     RJW 05/29/86  CREATED ORIGINAL TEST.
--     BCB 08/18/87  CHANGED HEADER TO STANDARD HEADER FORMAT.
--                   CORRECTED ERROR MESSAGES AND ADDED CALLS TO
--                   IDENT_STR.

WITH REPORT; USE REPORT;

PROCEDURE  C35507C  IS

     TYPE CHAR IS ('A', 'a');

     TYPE NEWCHAR IS NEW CHAR;

     FUNCTION IDENT (CH : CHAR) RETURN CHAR IS
     BEGIN
          RETURN CHAR'VAL (IDENT_INT (CHAR'POS (CH)));
     END IDENT;

     FUNCTION IDENT (CH : NEWCHAR) RETURN NEWCHAR IS
     BEGIN
          RETURN NEWCHAR'VAL (IDENT_INT (NEWCHAR'POS (CH)));
     END IDENT;

     PROCEDURE CHECK_BOUND (STR1, STR2 : STRING) IS
     BEGIN
          IF STR1'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR " & STR2 &
                        "'IMAGE ('" & STR1 & "')" );
          END IF;
     END CHECK_BOUND;

BEGIN

     TEST( "C35507C" , "CHECK THAT THE ATTRIBUTES 'IMAGE' AND " &
                       "'VALUE' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A CHARACTER TYPE" );

     BEGIN  -- (A).
          IF CHAR'IMAGE ('A') /= "'A'" THEN
               FAILED ( "INCORRECT IMAGE FOR CHAR'('A')" );
          END IF;

          CHECK_BOUND (CHAR'IMAGE ('A'), "CHAR");

          IF CHAR'IMAGE ('a') /= "'a'" THEN
               FAILED ( "INCORRECT IMAGE FOR CHAR'('a')" );
          END IF;

          CHECK_BOUND (CHAR'IMAGE ('a'), "CHAR");

          IF NEWCHAR'IMAGE ('A') /= "'A'" THEN
               FAILED ( "INCORRECT IMAGE FOR NEWCHAR'('A')" );
          END IF;

          CHECK_BOUND (NEWCHAR'IMAGE ('A'), "NEWCHAR");

          IF NEWCHAR'IMAGE ('a') /= "'a'" THEN
               FAILED ( "INCORRECT IMAGE FOR NEWCHAR'('a')" );
          END IF;

          CHECK_BOUND (NEWCHAR'IMAGE ('a'), "NEWCHAR");

          IF CHAR'IMAGE (IDENT ('A')) /= "'A'" THEN
               FAILED ( "INCORRECT IMAGE FOR CHAR'( IDENT ('A'))" );
          END IF;

          CHECK_BOUND (CHAR'IMAGE (IDENT ('A')), "IDENT OF CHAR");

          IF CHAR'IMAGE (IDENT ('a')) /= "'a'" THEN
               FAILED ( "INCORRECT IMAGE FOR CHAR'( IDENT ('a'))" );
          END IF;

          CHECK_BOUND (CHAR'IMAGE (IDENT ('a')), "IDENT OF CHAR");

          IF NEWCHAR'IMAGE (IDENT ('A')) /= "'A'" THEN
               FAILED ( "INCORRECT IMAGE FOR NEWCHAR'( IDENT ('A'))" );
          END IF;

          CHECK_BOUND (NEWCHAR'IMAGE (IDENT ('A')), "IDENT OF NEWCHAR");

          IF NEWCHAR'IMAGE (IDENT ('a')) /= "'a'" THEN
               FAILED ( "INCORRECT IMAGE FOR NEWCHAR'( IDENT ('a'))" );
          END IF;

          CHECK_BOUND (NEWCHAR'IMAGE (IDENT ('a')), "IDENT OF NEWCHAR");

          FOR CH IN CHARACTER'VAL (32) .. CHARACTER'VAL (126) LOOP
               IF CHARACTER'IMAGE (CH) /= ("'" & CH) & "'" THEN
                    FAILED ( "INCORRECT IMAGE FOR CHARACTER'(" &
                              CH & ")" );
               END IF;

               CHECK_BOUND (CHARACTER'IMAGE (CH), "CHARACTER");

          END LOOP;

          FOR CH IN CHARACTER'VAL (0) .. CHARACTER'VAL (31) LOOP
               CHECK_BOUND (CHARACTER'IMAGE (CH), "CHARACTER");
          END LOOP;

          CHECK_BOUND (CHARACTER'IMAGE (CHARACTER'VAL (127)),
                       "CHARACTER");

     END;

     ---------------------------------------------------------------

     DECLARE -- (B).

          SUBTYPE SUBCHAR IS CHARACTER
               RANGE CHARACTER'VAL (127) .. CHARACTER'VAL (127);
     BEGIN
          FOR CH IN CHARACTER'VAL (32) .. CHARACTER'VAL (126) LOOP
               IF SUBCHAR'VALUE (("'" & CH) & "'") /= CH THEN
                    FAILED ( "INCORRECT SUBCHAR'VALUE FOR " & CH );
               END IF;
          END LOOP;

          FOR CH IN CHARACTER'VAL (0) .. CHARACTER'VAL (31) LOOP
               IF SUBCHAR'VALUE (CHARACTER'IMAGE (CH)) /= CH THEN
                    FAILED ( "INCORRECT SUBCHAR'VALUE FOR " &
                              CHARACTER'IMAGE (CH) );
               END IF;
          END LOOP;

          IF SUBCHAR'VALUE (CHARACTER'IMAGE (CHARACTER'VAL (127))) /=
             CHARACTER'VAL (127) THEN
               FAILED ( "INCORRECT SUBCHAR'VALUE FOR " &
                        "CHARACTER'VAL (127)" );
          END IF;
     END;

     BEGIN
          IF CHAR'VALUE ("'A'") /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'(""'A'"")" );
          END IF;

          IF CHAR'VALUE ("'a'") /= 'a' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'(""'a'"")" );
          END IF;

          IF NEWCHAR'VALUE ("'A'") /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'(""'A'"")" );
          END IF;

          IF NEWCHAR'VALUE ("'a'") /= 'a' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'(""'a'"")" );
          END IF;
     END;

     BEGIN
          IF CHAR'VALUE (IDENT_STR("'A'")) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'(IDENT_STR" &
                        "(""'A'""))" );
          END IF;

          IF CHAR'VALUE (IDENT_STR("'a'")) /= 'a' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'(IDENT_STR" &
                        "(""'a'""))" );
          END IF;

          IF NEWCHAR'VALUE (IDENT_STR("'A'")) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'(IDENT_STR" &
                        "(""'A'""))" );
          END IF;

          IF NEWCHAR'VALUE (IDENT_STR("'a'")) /= 'a' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'(IDENT_STR" &
                        "(""'a'""))" );
          END IF;
     END;

     BEGIN
          IF CHAR'VALUE (IDENT_STR ("'B'")) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'VALUE (IDENT_STR (""'B'"")) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'VALUE (IDENT_STR (""'B'"")) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHAR'VALUE (IDENT_STR (""'B'""))" );
     END;

     BEGIN
          IF CHARACTER'VALUE (IDENT_CHAR (ASCII.HT) & "'A'") = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE " &
                        "(IDENT_CHAR (ASCII.HT) & ""'A'"") - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE " &
                        "(IDENT_CHAR (ASCII.HT) & ""'A'"") - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE " &
                        "(IDENT_CHAR (ASCII.HT) & ""'A'"")" );
     END;

     BEGIN
          IF CHARACTER'VALUE ("'B'" & IDENT_CHAR (ASCII.HT)) = 'B' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (""'B'"" & " &
                        "IDENT_CHAR (ASCII.HT)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (""'B'"" & " &
                        "IDENT_CHAR (ASCII.HT)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE (""'B'"" & " &
                        "IDENT_CHAR (ASCII.HT)) " );
     END;

     BEGIN
          IF CHARACTER'VALUE ("'C'" & IDENT_CHAR (ASCII.BEL)) = 'C'
             THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (""'C'"" & " &
                        "IDENT_CHAR (ASCII.BEL)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (""'C'"" & " &
                        "IDENT_CHAR (ASCII.BEL)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE (""'C'"" & " &
                        "IDENT_CHAR (ASCII.BEL))" );
     END;

     BEGIN
          IF CHARACTER'VALUE (IDENT_STR ("'")) = ''' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""'"")) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""'"")) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE (IDENT_STR (""'""))" );
     END;

     BEGIN
          IF CHARACTER'VALUE (IDENT_STR ("''")) = ''' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""''"")) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""''"")) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE (IDENT_STR (""''""))" );
     END;

     BEGIN
          IF CHARACTER'VALUE (IDENT_STR ("'A")) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""'A"")) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""'A"")) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE IDENT_STR (""'A""))" );
     END;

     BEGIN
          IF CHARACTER'VALUE (IDENT_STR ("A'")) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""A'"")) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""A'"")) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE (IDENT_STR (""A'""))" );
     END;

     BEGIN
          IF CHARACTER'VALUE (IDENT_STR ("'AB'")) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""'AB'"")) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED FOR " &
                        "CHARACTER'VALUE (IDENT_STR (""'AB'"")) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VALUE IDENT_STR (""'AB'""))" );
     END;

     RESULT;
END C35507C;
