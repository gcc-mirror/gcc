-- C4A012B.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR
--     A UNIVERSAL_REAL EXPRESSION IF DIVISION BY ZERO IS ATTEMPTED.

--     CHECK THAT CONSTRAINT_ERROR IS RAISED FOR
--     0.0 ** (-1) (OR ANY OTHER NEGATIVE EXPONENT VALUE).

-- HISTORY:
--     RJW 09/04/86  CREATED ORIGINAL TEST.
--     CJJ 09/04/87  ADDED PASS MESSAGE FOR RAISING NUMERIC_ERROR;
--                   MODIFIED CODE TO PREVENT COMPILER OPTIMIZING
--                   OUT THE TEST.
--     JET 12/31/87  ADDED MORE CODE TO PREVENT OPTIMIZATION.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY
--     JRL 02/29/96  Added code to check for value of Machine_Overflows; if
--                   False, test is inapplicable.

WITH REPORT; USE REPORT;

PROCEDURE C4A012B IS

     F : FLOAT;

     I3 : INTEGER := -3;

     SUBTYPE SINT IS INTEGER RANGE -10 .. 10;
     SI5 : CONSTANT SINT := -5;

     FUNCTION IDENT (X:FLOAT) RETURN FLOAT IS
     BEGIN
          IF EQUAL (3,3) THEN
               RETURN X;
          ELSE
               RETURN 1.0;
          END IF;
     END IDENT;

BEGIN

     TEST ( "C4A012B", "CHECK THAT CONSTRAINT_ERROR " &
                       "IS RAISED FOR " &
                       "0.0 ** (-1) (OR ANY OTHER NEGATIVE EXPONENT " &
                       "VALUE)" );

     IF FLOAT'MACHINE_OVERFLOWS = FALSE THEN
        REPORT.NOT_APPLICABLE ("Float'Machine_Overflows = False");
     ELSE

        BEGIN
             F := IDENT (0.0) ** (-1);
             FAILED ( "THE EXPRESSION '0.0 ** (-1)' DID NOT RAISE " &
                      "AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 1");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** (-1)' RAISED THE " &
                           "WRONG EXCEPTION" );
        END;

        BEGIN
             F := 0.0 ** (IDENT_INT (-1));
             FAILED ( "THE EXPRESSION '0.0 ** (IDENT_INT (-1))' DID " &
                       "NOT RAISE AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 2");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** (IDENT_INT (-1))' " &
                           "RAISED THE WRONG EXCEPTION" );
        END;

        BEGIN
             F := 0.0 ** (INTEGER'POS (IDENT_INT (-1)));
             FAILED ( "THE EXPRESSION '0.0 ** " &
                      "(INTEGER'POS (IDENT_INT (-1)))' DID " &
                      "NOT RAISE AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 3");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** " &
                           "(INTEGER'POS (IDENT_INT (-1)))' RAISED " &
                           "THE WRONG EXCEPTION" );
        END;

        BEGIN
             F := IDENT(0.0) ** I3;
             FAILED ( "THE EXPRESSION '0.0 ** I3' DID NOT RAISE " &
                       "AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 4");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** I3' RAISED THE " &
                           "WRONG EXCEPTION" );
        END;

        BEGIN
             F := 0.0 ** (IDENT_INT (I3));
             FAILED ( "THE EXPRESSION '0.0 ** (IDENT_INT (I3))' DID " &
                      "NOT RAISE AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 5");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** (IDENT_INT (I3))' " &
                            "RAISED THE WRONG EXCEPTION" );
        END;

        BEGIN
             F := IDENT (0.0) ** SI5;
             FAILED ( "THE EXPRESSION '0.0 ** SI5' DID NOT RAISE " &
                       "AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 6");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** SI5' RAISED THE " &
                           "WRONG EXCEPTION" );
        END;

        BEGIN
             F := 0.0 ** (IDENT_INT (SI5));
             FAILED ( "THE EXPRESSION '0.0 ** (IDENT_INT (SI5))' DID " &
                      "NOT RAISE AN EXCEPTION" );
             IF EQUAL ( INTEGER(F), INTEGER(F) ) THEN
                  COMMENT ("SHOULDN'T BE HERE!");
             END IF;
        EXCEPTION
             WHEN CONSTRAINT_ERROR =>
                  COMMENT ("CONSTRAINT_ERROR RAISED - 7");
             WHEN OTHERS =>
                  FAILED ( "THE EXPRESSION '0.0 ** (IDENT_INT (SI5))' " &
                            "RAISED THE WRONG EXCEPTION" );
        END;

     END IF;

     RESULT;

END C4A012B;
