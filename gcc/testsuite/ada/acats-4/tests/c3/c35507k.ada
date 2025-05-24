-- C35507K.ADA

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
--     CHECK THAT THE ATTRIBUTES 'POS' AND 'VAL' YIELD THE CORRECT
--     RESULTS WHEN THE PREFIX IS A CHARACTER TYPE.

-- HISTORY:
--     RJW 06/03/86
--     JLH 07/28/87  MODIFIED FUNCTION IDENT.
-- PWN 11/30/94 REMOVED PART OF TEST INVALID FOR ADA 9X.

WITH REPORT; USE REPORT;

PROCEDURE  C35507K  IS

     TYPE CHAR IS ('A', B);

     TYPE NEWCHAR IS NEW CHAR;

     SUBTYPE SCHAR IS CHARACTER
             RANGE CHARACTER'VAL (127) .. CHARACTER'VAL (127);

     BLANK : CONSTANT CHARACTER := ' ';

     POSITION : INTEGER;

     NONGRAPH : ARRAY (0 .. 31) OF CHARACTER :=
          (ASCII.NUL, ASCII.SOH, ASCII.STX, ASCII.ETX,
           ASCII.EOT, ASCII.ENQ, ASCII.ACK, ASCII.BEL,
           ASCII.BS,  ASCII.HT,  ASCII.LF,  ASCII.VT,
           ASCII.FF,  ASCII.CR,  ASCII.SO,  ASCII.SI,
           ASCII.DLE, ASCII.DC1, ASCII.DC2, ASCII.DC3,
           ASCII.DC4, ASCII.NAK, ASCII.SYN, ASCII.ETB,
           ASCII.CAN, ASCII.EM,  ASCII.SUB, ASCII.ESC,
           ASCII.FS,  ASCII.GS,  ASCII.RS,  ASCII.US);

     FUNCTION IDENT (CH : CHAR) RETURN CHAR IS
     BEGIN
          IF EQUAL (CHAR'POS (CH), CHAR'POS (CH)) THEN
               RETURN CH;
          END IF;
          RETURN CHAR'FIRST;
     END IDENT;

     FUNCTION IDENT (CH : NEWCHAR) RETURN NEWCHAR IS
     BEGIN
          IF EQUAL (NEWCHAR'POS (CH), NEWCHAR'POS (CH)) THEN
               RETURN CH;
          END IF;
          RETURN NEWCHAR'FIRST;
     END IDENT;

BEGIN

     TEST( "C35507K" , "CHECK THAT THE ATTRIBUTES 'POS' AND " &
                       "'VAL' YIELD THE CORRECT RESULTS WHEN THE " &
                       "PREFIX IS A CHARACTER TYPE" );

     BEGIN
          IF CHAR'POS ('A') /= 0 THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'POS('A') - 1" );
          END IF;

          IF CHAR'POS (B) /= 1 THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'POS(B) - 1" );
          END IF;

          IF CHAR'VAL (0) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'VAL(0)" );
          END IF;

          IF CHAR'VAL (1) /= B THEN
               FAILED ( "INCORRECT VALUE FOR CHAR'VAL(1)" );
          END IF;

          IF CHAR'POS (IDENT ('A')) /= 0 THEN
               FAILED ( "INCORRECT VALUE " &
                        "FOR CHAR'POS (IDENT ('A')) - 2" );
          END IF;

          IF CHAR'POS (IDENT (B)) /= 1 THEN
               FAILED ( "INCORRECT VALUE " &
                        "FOR CHAR'POS (IDENT (B)) - 2" );
          END IF;

     END;

     BEGIN
          IF NEWCHAR'POS ('A') /= 0 THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'POS('A')" );
          END IF;

          IF NEWCHAR'POS (B) /= 1 THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'POS(B) - 1" );
          END IF;

          IF NEWCHAR'VAL (0) /= 'A' THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'VAL(0) - 1" );
          END IF;

          IF NEWCHAR'VAL (1) /= B THEN
               FAILED ( "INCORRECT VALUE FOR NEWCHAR'VAL(1)" );
          END IF;

          IF NEWCHAR'VAL (IDENT_INT (1)) /= B THEN
               FAILED ( "INCORRECT VALUE " &
                        "FOR NEWCHAR'POS (IDENT (B)) - 2" );
          END IF;

          IF (NEWCHAR'VAL (IDENT_INT(0))) /= 'A' THEN
               FAILED ( "INCORRECT VALUE " &
                        "FOR IDENT (NEWCHAR'VAL (0)) - 2" );
          END IF;

     END;

     BEGIN
          IF CHAR'VAL (IDENT_INT (2)) = B THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'VAL (IDENT_INT (2)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHAR'VAL (IDENT_INT (2)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                         "FOR CHAR'VAL (IDENT_INT (2))" );
     END;

     BEGIN
          IF NEWCHAR'VAL (IDENT_INT (-1)) = 'A' THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR NEWCHAR'VAL (IDENT_INT (-1)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR NEWCHAR'VAL (IDENT_INT (-1)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR NEWCHAR'VAL (IDENT_INT (-1))" );
     END;

     POSITION := 0;

     FOR CH IN CHARACTER LOOP
          IF SCHAR'POS (CH) /= POSITION THEN
               FAILED ( "INCORRECT VALUE FOR SCHAR'POS OF " &
                         CHARACTER'IMAGE (CH) );
          END IF;

          POSITION := POSITION + 1;
     END LOOP;

     FOR POSITION IN 0 .. 31 LOOP
          IF CHARACTER'VAL (POSITION) /= NONGRAPH (POSITION) THEN
               FAILED ( "INCORRECT VALUE FOR CHARACTER'VAL OF " &
                        "NONGRAPHIC CHARACTER IN POSITION - " &
                         INTEGER'IMAGE (POSITION) );
          END IF;
     END LOOP;

     POSITION := 32;

     FOR CH IN BLANK .. ASCII.TILDE LOOP
          IF SCHAR'VAL (POSITION) /= CH THEN
               FAILED ( "INCORRECT VALUE FOR SCHAR'VAL OF " &
                        "GRAPHIC CHARACTER IN POSITION - " &
                         INTEGER'IMAGE (POSITION) );
          END IF;

          POSITION := POSITION + 1;
     END LOOP;

     IF CHARACTER'VAL (127) /= ASCII.DEL THEN
          FAILED ( "INCORRECT VALUE FOR CHARACTER'VAL OF " &
                    "NONGRAPHIC CHARACTER IN POSITION - 127" );
     END IF;

     BEGIN
          IF CHARACTER'VAL (IDENT_INT (-1)) = ASCII.NUL THEN
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHARACTER'VAL (IDENT_INT (-1)) - 1" );
          ELSE
               FAILED ( "NO EXCEPTION RAISED " &
                        "FOR CHARACTER'VAL (IDENT_INT (-1)) - 2" );
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED " &
                        "FOR CHARACTER'VAL (IDENT_INT (-1))" );
     END;

     RESULT;
END C35507K;
