-- C46014A.ADA

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
--     FOR PREDEFINED TYPE INTEGER, CHECK THAT
--     CONSTRAINT_ERROR IS RAISED IF THE OPERAND VALUE OF A
--     CONVERSION LIES OUTSIDE OF THE RANGE OF THE TARGET TYPE'S BASE
--     TYPE. ALSO, CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE
--     OPERAND VALUE LIES OUTSIDE OF THE RANGE OF THE TARGET TYPE'S
--     SUBTYPE BUT WITHIN THE RANGE OF THE BASE TYPE.

-- HISTORY:
--     RJW 09/08/86  CREATED ORIGINAL TEST.
--     RJW 11/13/87  ADDED CODE TO PREVENT DEAD VARIABLE OPTIMIZATION.
--     JET 12/30/87  ADDED MORE CODE TO PREVENT OPTIMIZATION.
--     MRM 03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY
--     JRL 12/08/96  Changed usages of System.Max_Int and System.Min_Int to
--                   Integer'Base'Last and Integer'Base'First in first two
--                   subtests.

WITH REPORT; USE REPORT;
PROCEDURE C46014A IS

     SUBTYPE SMALL IS INTEGER RANGE -100 .. 100;
     S1 : SMALL;

     TYPE INT IS RANGE -100 .. 100;
     T1 : INT;

     TYPE NEWINTEGER IS NEW INTEGER;
     N1 : NEWINTEGER;

     SUBTYPE SUBNEW IS NEWINTEGER RANGE -100 .. 100;
     SN : SUBNEW;

     I1 : INTEGER;
     P1 : POSITIVE;
     L1 : NATURAL;

     FUNCTION IDENT (I : INTEGER) RETURN INT IS
     BEGIN
          RETURN INT'VAL (IDENT_INT (I));
     END IDENT;

     FUNCTION IDENT (I : NEWINTEGER) RETURN NEWINTEGER IS
     BEGIN
          RETURN NEWINTEGER'VAL (IDENT_INT (NEWINTEGER'POS (I)));
     END IDENT;

BEGIN
     TEST ( "C46014A", "FOR PREDEFINED TYPE INTEGER, CHECK THAT " &
                       "CONSTRAINT_ERROR IS RAISED IF " &
                       "THE OPERAND VALUE OF A CONVERSION LIES " &
                       "OUTSIDE OF THE RANGE OF THE TARGET TYPE'S " &
                       "BASE TYPE. ALSO, CHECK THAT " &
                       "CONSTRAINT_ERROR IS RAISED IF THE OPERAND " &
                       "VALUE LIES OUTSIDE OF THE RANGE OF THE " &
                       "TARGET TYPE'S SUBTYPE BUT WITHIN THE " &
                       "RANGE OF THE BASE TYPE" );

     BEGIN
          I1 := Integer'Base'Last + Ident_Int(1);
          Failed ("NO EXCEPTION RAISED FOR INTEGER'BASE'LAST + 1");
          IF EQUAL (I1, I1) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               Comment ("CONSTRAINT_ERROR RAISED FOR INTEGER'BASE'LAST + 1");
          WHEN OTHERS =>
               Failed ("WRONG EXCEPTION RAISED FOR INTEGER'BASE'LAST + 1");
     END;

     BEGIN
          I1 := Integer'Base'First - Ident_Int(1);
          Failed ("NO EXCEPTION RAISED FOR INTEGER'BASE'FIRST - 1");
          IF EQUAL (I1, I1) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               Comment ("CONSTRAINT_ERROR RAISED FOR INTEGER'BASE'FIRST - 1");
          WHEN OTHERS =>
               Failed ("WRONG EXCEPTION RAISED FOR INTEGER'BASE'FIRST - 1");
     END;

     BEGIN
          I1 := INTEGER (IDENT_INT (INTEGER'FIRST) - 1);
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "INTEGER (IDENT_INT (INTEGER'FIRST) - 1)" );
          IF EQUAL (I1, I1) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED FOR " &
                         "INTEGER (IDENT_INT (INTEGER'FIRST - 1)" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "INTEGER (IDENT_INT (INTEGER'FIRST - 1)" );
     END;

     BEGIN
          N1 := NEWINTEGER (IDENT_INT (INTEGER'LAST) + 1);
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "NEWINTEGER (IDENT_INT (INTEGER'LAST) + 1)" );
          IF EQUAL (INTEGER (N1), INTEGER (N1)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED FOR " &
                         "NEWINTEGER (IDENT_INT (INTEGER'LAST + 1)" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "NEWINTEGER (IDENT_INT (INTEGER'LAST + 1)" );
     END;

     BEGIN
          T1 := INT (INT'BASE'FIRST - IDENT (1));
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "INT (INT'BASE'FIRST - IDENT (1))" );
          IF EQUAL (INTEGER (T1), INTEGER (T1)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT ( "CONSTRAINT_ERROR RAISED FOR " &
                         "INT (INT'BASE'FIRST - IDENT (1))" );
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "INT (INT'BASE'FIRST - IDENT (1))" );
     END;

     BEGIN
          T1 := IDENT (-101);
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "T1 := -101" );
          IF EQUAL (INTEGER (T1), INTEGER (T1)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "T1 := -101" );
     END;

     BEGIN
          T1 := INTEGER'POS (IDENT_INT (101));
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "T1 := INTEGER'POS (IDENT_INT (101))" );
          IF EQUAL (INTEGER (T1), INTEGER (T1)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "T1 := INTEGER'POS (IDENT_INT (101));" );
     END;

     BEGIN
          T1 := INT (IDENT (INTEGER (INT'FIRST)) - 1);
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "INT (INT'FIRST - 1)" );
          IF EQUAL (INTEGER (T1), INTEGER (T1)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "INT (INT'FIRST - 1)" );
     END;

     BEGIN
          T1 := INT (IDENT_INT (101));
          FAILED ( "NO EXCEPTION RAISED FOR INT (101)" );
          IF EQUAL (INTEGER (T1), INTEGER (T1)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR INT (101)" );
     END;

     BEGIN
          S1 := SMALL (IDENT_INT (101));
          FAILED ( "NO EXCEPTION RAISED FOR SMALL (101)" );
          IF EQUAL (S1, S1) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR SMALL (101)" );
     END;

     BEGIN
          SN := SUBNEW (IDENT_INT (-101));
          FAILED ( "NO EXCEPTION RAISED FOR SUBNEW (-101)" );
          IF EQUAL (INTEGER (SN), INTEGER (SN)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR SUBNEW (-101)" );
     END;

     BEGIN
          P1 := IDENT_INT (101);
          SN := SUBNEW (P1);
          FAILED ( "NO EXCEPTION RAISED FOR SUBNEW (P1)" );
          IF EQUAL (INTEGER (SN), INTEGER (SN)) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR SUBNEW (P1)" );
     END;

     BEGIN
          SN := IDENT (0);
          P1 := POSITIVE (SN);
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "POSITIVE (SN)" );
          IF EQUAL (P1, P1) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "POSITIVE (SN)" );
     END;

     BEGIN
          N1 := IDENT (-1);
          L1 := NATURAL (N1);
          FAILED ( "NO EXCEPTION RAISED FOR " &
                   "NATURAL (N1)" );
          IF EQUAL (L1, L1) THEN
               COMMENT ("SHOULDN'T GET HERE");
          END IF;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ( "WRONG EXCEPTION RAISED FOR " &
                        "NATURAL (N1)" );
     END;

     RESULT;
END C46014A;
