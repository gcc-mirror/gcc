-- C35503C.ADA

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
--     THE PREFIX IS AN INTEGER TYPE.
--     SUBTESTS ARE :
--         PART (A). TESTS FOR 'IMAGE'.
--         PART (B). TESTS FOR 'VALUE'.

-- HISTORY:
--     RJW  03/17/86  CREATED ORIGINAL TEST.
--     VCL  10/23/87  MODIFIED THIS HEADER, ADDED A CHECK THAT
--                    CONSTRAINT_ERROR IS RAISED FOR THE ATTRIBUTE
--                    'VALUE' IF THE FINAL SHARP OR COLON IS MISSING
--                    FROM A BASED LITERAL.

WITH REPORT; USE REPORT;
PROCEDURE C35503C IS
     TYPE NEWINT IS NEW INTEGER;
     TYPE INT IS RANGE -1000 .. 1000;

     FUNCTION IDENT (X : INT) RETURN INT IS
     BEGIN
          IF EQUAL (INT'POS (X), INT'POS(X)) THEN
               RETURN X;
          END IF;
          RETURN INT'FIRST;
     END IDENT;

BEGIN
     TEST ("C35503C", "THE ATTIBUTES 'IMAGE' AND 'VALUE' YIELD THE " &
                      "CORRECT RESULTS WHEN THE PREFIX IS AN " &
                      "INTEGER TYPE" );
-- PART (A).

     BEGIN
          IF INTEGER'IMAGE (-500) /= "-500" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-500'" );
          END IF;
          IF INTEGER'IMAGE (-500)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-500'" );
          END IF;

          IF NEWINT'IMAGE (2 ** 6) /= " 64" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '2 ** 6'" );
          END IF;
          IF NEWINT'IMAGE (2 ** 6)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '2 ** 6'" );
          END IF;

          IF NATURAL'IMAGE (-1E2) /= "-100" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-1E2'" );
          END IF;
          IF NATURAL'IMAGE (-1E2)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-1E2'" );
          END IF;

          IF NEWINT'IMAGE (3_45) /= " 345" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '3_45'" );
          END IF;
          IF NEWINT'IMAGE (3_45)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '3_45'" );
          END IF;

          IF INTEGER'IMAGE (-2#1111_1111#) /= "-255" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-2#1111_1111#'" );
          END IF;
          IF INTEGER'IMAGE (-2#1111_1111#)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-2#1111_1111#'" );
          END IF;

          IF NEWINT'IMAGE (16#FF#) /= " 255" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '16#FF#'" );
          END IF;
          IF NEWINT'IMAGE (16#FF#)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '16#FF#'" );
          END IF;

          IF INTEGER'IMAGE (-016#0FF#) /= "-255" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-016#0FF#'" );
          END IF;
          IF INTEGER'IMAGE (-016#0FF#)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-016#0FF#'" );
          END IF;

          IF NEWINT'IMAGE (2#1110_0000#) /= " 224" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '2#1110_0000#'" );
          END IF;
          IF NEWINT'IMAGE (2#1110_0000#)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '2#1110_0000#'" );
          END IF;

          IF POSITIVE'IMAGE (-16#E#E1) /= "-224" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-16#E#E1'" );
          END IF;
          IF POSITIVE'IMAGE (-16#E#E1)'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-16#E#E1'" );
          END IF;

          IF INT'IMAGE (IDENT(-1000)) /= "-1000" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-1000'" );
          END IF;
          IF INT'IMAGE (IDENT(-1000))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-1000'" );
          END IF;

          IF INT'IMAGE (IDENT(-999)) /= "-999" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-999'" );
          END IF;
          IF INT'IMAGE (IDENT(-999))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-999'" );
          END IF;

          IF INT'IMAGE (IDENT(-10)) /= "-10" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-1000'" );
          END IF;
          IF INT'IMAGE (IDENT(-10))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-10'" );
          END IF;

          IF INT'IMAGE (IDENT(-9)) /= "-9" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-9'" );
          END IF;
          IF INT'IMAGE (IDENT(-9))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-9'" );
          END IF;

          IF INT'IMAGE (IDENT(-1)) /= "-1" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '-1'" );
          END IF;
          IF INT'IMAGE (IDENT(-1))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '-1'" );
          END IF;

          IF INT'IMAGE (IDENT(0)) /= " 0" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '0'" );
          END IF;
          IF INT'IMAGE (IDENT(0))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '0'" );
          END IF;

          IF INT'IMAGE (IDENT(1)) /= " 1" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '1'" );
          END IF;
          IF INT'IMAGE (IDENT(1))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '1'" );
          END IF;

          IF INT'IMAGE (IDENT(9)) /= " 9" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '9'" );
          END IF;
          IF INT'IMAGE (IDENT(9))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '9'" );
          END IF;

          IF INT'IMAGE (IDENT(10)) /= " 10" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '10'" );
          END IF;
          IF INT'IMAGE (IDENT(10))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '10'" );
          END IF;

          IF INT'IMAGE (IDENT(999)) /= " 999" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '999'" );
          END IF;
          IF INT'IMAGE (IDENT(999))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '999'" );
          END IF;

          IF INT'IMAGE (IDENT(1000)) /= " 1000" THEN
               FAILED ( "INCORRECT 'IMAGE' OF '1000'" );
          END IF;
          IF INT'IMAGE (IDENT(1000))'FIRST /= 1 THEN
               FAILED ( "INCORRECT LOWER BOUND FOR '1000'" );
          END IF;

     END;

-----------------------------------------------------------------------

-- PART (B).

     BEGIN
          IF POSITIVE'VALUE (IDENT_STR("-500")) /= -500 THEN
               FAILED ( "INCORRECT 'VALUE' OF ""-500""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF ""-500""" );
     END;

     BEGIN
          IF NEWINT'VALUE (" -001E2") /= -100 THEN
               FAILED ( "INCORRECT 'VALUE' OF "" -001E2""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF "" -001E2""" );
     END;

     BEGIN
          IF INTEGER'VALUE ("03_45") /= 345 THEN
               FAILED ( "INCORRECT 'VALUE' OF ""03_45""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF ""03_45""" );
     END;

     BEGIN
          IF NEWINT'VALUE ("-2#1111_1111#") /= -255 THEN
               FAILED ( "INCORRECT 'VALUE' OF ""-2#1111_1111#""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF "&
                        """-2#1111_1111#""" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("16#FF#")) /= 255 THEN
               FAILED ( "INCORRECT 'VALUE' OF ""16#FF#""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF ""16#FF#""" );
     END;

     BEGIN
          IF NATURAL'VALUE (IDENT_STR("-016#0FF#")) /= -255 THEN
               FAILED ( "INCORRECT 'VALUE' OF ""-016#0FF#""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF " &
                        """-016#0FF#""" );
     END;

     BEGIN
          IF INTEGER'VALUE ("2#1110_0000#     ") /= 224 THEN
               FAILED ( "INCORRECT 'VALUE' OF " &
                        """2#1110_0000#     """ );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF " &
                        """2#1110_0000#     """ );
     END;

     BEGIN
          IF NEWINT'VALUE ("  -16#E#E1") /= -224 THEN
               FAILED ( "INCORRECT 'VALUE' OF ""  -16#E#E1""" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - 'VALUE' OF " &
                        """  -16#E#E1""" );
     END;

     BEGIN
          IF INTEGER'VALUE ("5/0") = 0 THEN
               FAILED ( "NO EXCEPTION RAISED - ""5/0"" - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - ""5/0"" - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - ""5/0""" );
     END;

     DECLARE
          SUBTYPE SUBINT IS INTEGER RANGE 0 .. 10;
     BEGIN
          IF SUBINT'VALUE (IDENT_STR("-500")) /= -500 THEN
               FAILED ( "INCORRECT VALUE WITH ""-500"" AND SUBINT" );
          END IF;
     EXCEPTION
          WHEN OTHERS =>
               FAILED ( "EXCEPTION RAISED - SUBINT" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("1.0")) = 1 THEN
               FAILED ( "NO EXCEPTION RAISED - "" 1.0"" - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - ""1.0"" - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - ""1.0"" " );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_CHAR(ASCII.HT) & "244") /= 244 THEN
               FAILED ( "NO EXCEPTION RAISED - LEADING 'HT' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - LEADING 'HT' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - LEADING 'HT'" );
     END;

     BEGIN
          IF INTEGER'VALUE ("244" & (IDENT_CHAR(ASCII.HT))) /= 244 THEN
               FAILED ( "NO EXCEPTION RAISED - TRAILING 'HT' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - TRAILING 'HT' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - TRAILING 'HT'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("2__44")) /= 244 THEN
               FAILED ( "NO EXCEPTION RAISED - CONSECUTIVE '_' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - CONSECUTIVE '_' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "WITH CONSECUTIVE '_'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("_244")) /= 244 THEN
               FAILED ( "NO EXCEPTION RAISED - LEADING '_' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - LEADING '_' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - LEADING '_'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("244_")) /= 244 THEN
               FAILED ( "NO EXCEPTION RAISED - TRAILING '_' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - TRAILING '_' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - TRAILING '_'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("244_E1")) /= 2440 THEN
               FAILED ( "NO EXCEPTION RAISED - '_' BEFORE 'E' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - '_' BEFORE 'E' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - '_' BEFORE 'E'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("244E_1")) /= 2440 THEN
               FAILED ( "NO EXCEPTION RAISED - '_' " &
                        "FOLLOWING 'E' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - '_' FOLLOWING 'E' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "- '_' FOLLOWING 'E'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("244_e1")) /= 2440 THEN
               FAILED ( "NO EXCEPTION RAISED - '_' BEFORE 'e' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - '_' BEFORE 'e' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - '_' BEFORE 'e'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("16#_FF#")) /= 255 THEN
               FAILED ( "NO EXCEPTION RAISED - LEADING '_' IN BASED " &
                        "LITERAL - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - LEADING '_' IN BASED " &
                        "LITERAL - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "- LEADING '_' IN BASED LITERAL" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("1E-0")) /= 1 THEN
               FAILED ( "NO EXCEPTION RAISED - NEGATIVE " &
                        "EXPONENT - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - NEGATIVE EXPONENT - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "- NEGATIVE EXPONENT" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("244.")) /= 244 THEN
               FAILED ( "NO EXCEPTION RAISED - TRAILING '.' - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - TRAILING '.' - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - TRAILING '.'" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("8#811#")) /= 0 THEN
               FAILED ( "NO EXCEPTION RAISED - " &
                        "DIGITS NOT IN CORRECT RANGE - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - " &
                        "DIGITS NOT IN CORRECT RANGE - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED - " &
                        "DIGITS NOT IN CORRECT RANGE" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("1#000#")) /= 0 THEN
               FAILED ( "NO EXCEPTION RAISED - BASE LESS THAN 2 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED - BASE LESS THAN 2 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "- BASE LESS THAN 2" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("17#0#")) /= 0 THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "- BASE GREATER THAN 16 - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "- BASE GREATER THAN 16 - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "- BASE GREATER THAN 16" );
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("8#666")) /= 438 THEN
               FAILED ("NO EXCEPTION RAISED - MISSING FINAL SHARP - 1");
          ELSE
               FAILED ("NO EXCEPTION RAISED - MISSING FINAL SHARP - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - MISSING FINAL SHARP");
     END;

     BEGIN
          IF INTEGER'VALUE (IDENT_STR("16:FF")) /= 255 THEN
               FAILED ("NO EXCEPTION RAISED - MISSING FINAL COLON - 1");
          ELSE
               FAILED ("NO EXCEPTION RAISED - MISSING FINAL COLON - 2");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED - MISSING FINAL COLON");
     END;

     RESULT;
END C35503C;
