-- C37009A.ADA

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
--     CHECK THAT AN UNCONSTRAINED RECORD TYPE CAN BE USED TO DECLARE A
--     RECORD COMPONENT THAT CAN BE INITIALIZED WITH AN APPROPRIATE
--     EXPLICIT OR DEFAULT VALUE.

-- HISTORY:
--     DHH 02/01/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C37009A IS

     TYPE FLOAT IS DIGITS 5;
     TYPE COLOR IS (RED, YELLOW, BLUE);

     TYPE COMPONENT IS
          RECORD
               I : INTEGER := 1;
               X : FLOAT := 3.5;
               BOL : BOOLEAN := FALSE;
               FIRST : COLOR := RED;
          END RECORD;
     TYPE COMP_DIS(A : INTEGER := 1) IS
          RECORD
               I : INTEGER := 1;
               X : FLOAT := 3.5;
               BOL : BOOLEAN := FALSE;
               FIRST : COLOR := RED;
          END RECORD;
     SUBTYPE SMAL_INTEGER IS INTEGER RANGE 1 .. 10;
     TYPE LIST IS ARRAY(INTEGER RANGE <>) OF FLOAT;

     TYPE DISCRIM(P : SMAL_INTEGER := 2) IS
          RECORD
               A : LIST(1 .. P) := (1 .. P => 1.25);
          END RECORD;

     TYPE REC_T IS                                     -- EXPLICIT INIT.
          RECORD
               T : COMPONENT := (5, 6.0, TRUE, YELLOW);
               U : DISCRIM(3) := (3, (1 .. 3 => 2.25));
               L : COMP_DIS(5) := (A => 5, I => 5, X => 6.0,
                                      BOL =>TRUE, FIRST => YELLOW);
          END RECORD;

     TYPE REC_DEF_T IS                                 -- DEFAULT INIT.
          RECORD
               T : COMPONENT;
               U : DISCRIM;
               L : COMP_DIS;
          END RECORD;

     REC : REC_T;
     REC_DEF : REC_DEF_T;

     FUNCTION IDENT_FLT(X : FLOAT) RETURN FLOAT IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN X;
          ELSE
               RETURN 0.0;
          END IF;
     END IDENT_FLT;

     FUNCTION IDENT_ENUM(X : COLOR) RETURN COLOR IS
     BEGIN
          IF EQUAL(3,3) THEN
               RETURN X;
          ELSE
               RETURN BLUE;
          END IF;
     END IDENT_ENUM;

BEGIN
     TEST("C37009A", "CHECK THAT AN UNCONSTRAINED RECORD TYPE CAN " &
                     "BE USED TO DECLARE A RECORD COMPONENT THAT " &
                     "CAN BE INITIALIZED WITH AN APPROPRIATE " &
                     "EXPLICIT OR DEFAULT VALUE");

     IF REC_DEF.T.I /= IDENT_INT(1) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF INTEGER");
     END IF;

     IF IDENT_BOOL(REC_DEF.T.BOL) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF BOOLEAN");
     END IF;

     IF REC_DEF.T.X /= IDENT_FLT(3.5) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF REAL");
     END IF;

     IF REC_DEF.T.FIRST /= IDENT_ENUM(RED) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF ENUMERATION");
     END IF;

     FOR I IN 1 .. 2 LOOP
          IF REC_DEF.U.A(I) /= IDENT_FLT(1.25) THEN
               FAILED("INCORRECT DEFAULT INITIALIZATION OF ARRAY " &
                      "POSITION " & INTEGER'IMAGE(I));
          END IF;
     END LOOP;

     IF REC_DEF.L.A /= IDENT_INT(1) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF DISCRIMINANT " &
                 "- L");
     END IF;

     IF REC_DEF.L.I /= IDENT_INT(1) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF INTEGER - L");
     END IF;

     IF IDENT_BOOL(REC_DEF.L.BOL) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF BOOLEAN - L");
     END IF;

     IF REC_DEF.L.X /= IDENT_FLT(3.5) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF REAL - L");
     END IF;

     IF REC_DEF.L.FIRST /= IDENT_ENUM(RED) THEN
          FAILED("INCORRECT DEFAULT INITIALIZATION OF ENUMERATION - L");
     END IF;
--------------------------------------------------------------------
     IF REC.T.I /= IDENT_INT(5) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF INTEGER");
     END IF;

     IF NOT IDENT_BOOL(REC.T.BOL) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF BOOLEAN");
     END IF;

     IF REC.T.X /= IDENT_FLT(6.0) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF REAL");
     END IF;

     IF REC.T.FIRST /= YELLOW THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF ENUMERATION");
     END IF;

     FOR I IN 1 .. 3 LOOP
          IF REC.U.A(I) /= IDENT_FLT(2.25) THEN
               FAILED("INCORRECT EXPLICIT INITIALIZATION OF ARRAY " &
                      "POSITION " & INTEGER'IMAGE(I));
          END IF;
     END LOOP;

     IF REC.L.A /= IDENT_INT(5) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF DISCRIMINANT " &
                 "- L");
     END IF;

     IF REC.L.I /= IDENT_INT(5) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF INTEGER - L");
     END IF;

     IF NOT IDENT_BOOL(REC.L.BOL) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF BOOLEAN - L");
     END IF;

     IF REC.L.X /= IDENT_FLT(6.0) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF REAL - L");
     END IF;

     IF REC.L.FIRST /= IDENT_ENUM(YELLOW) THEN
          FAILED("INCORRECT EXPLICIT INITIALIZATION OF ENUMERATION " &
                 "- L");
     END IF;

     RESULT;

END C37009A;
