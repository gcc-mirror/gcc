-- C83051A.ADA

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
--     CHECK THAT DECLARATIONS IN THE VISIBLE PART OF A PACKAGE NESTED
--     WITHIN THE VISIBLE PART OF A PACKAGE ARE VISIBLE BY SELECTION
--     FROM OUTSIDE THE OUTERMOST PACKAGE.

-- HISTORY:
--     GMT 09/07/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C83051A IS

BEGIN
     TEST ("C83051A", "CHECK THAT DECLARATIONS IN THE VISIBLE " &
                      "PART OF A PACKAGE NESTED WITHIN THE VISIBLE " &
                      "PART OF A PACKAGE ARE VISIBLE BY SELECTION " &
                      "FROM OUTSIDE THE OUTERMOST PACKAGE");
     A_BLOCK:
     DECLARE
          PACKAGE APACK IS
               PACKAGE BPACK  IS
                    TYPE    T1  IS (RED,GREEN);
                    TYPE    T2A IS ('A', 'B', 'C', 'D');
                    TYPE    T3  IS NEW BOOLEAN;
                    TYPE    T4  IS NEW INTEGER RANGE -3 .. 8;
                    TYPE    T5  IS DIGITS 5;
                    TYPE    T67 IS DELTA 0.5 RANGE -2.0 .. 10.0;
                    TYPE    T9A IS ARRAY (INTEGER RANGE <>) OF T3;
                    SUBTYPE T9B IS T9A (1..10);
                    TYPE    T9C IS ACCESS T9B;
                    TYPE    T10 IS PRIVATE;
                    V1       : T3 := FALSE;
                    ZERO     : CONSTANT T4 := 0;
                    A_FLT    : T5 := 3.0;
                    A_FIX    : T67 := -1.0;
                    ARY      : T9A(1..4) := (TRUE,TRUE,TRUE,FALSE);
                    P1 : T9C := NEW T9B'( 1..5  => T3'(TRUE),
                                          6..10 => T3'(FALSE) );
                    C1 : CONSTANT T10;

                    FUNCTION RET_T1 (X : T1) RETURN T1;

                    FUNCTION RET_CHAR (X : CHARACTER) RETURN T10;

                    GENERIC
                    PROCEDURE DO_NOTHING (X : IN OUT T3);
               PRIVATE
                    TYPE T10 IS NEW CHARACTER;
                    C1 : CONSTANT T10 := 'J';
               END BPACK;
          END APACK;

     PACKAGE BODY APACK IS
          PACKAGE BODY BPACK IS
               FUNCTION RET_T1 (X : T1) RETURN T1 IS
               BEGIN
                    IF X = RED THEN
                         RETURN GREEN;
                    ELSE
                         RETURN RED;
                    END IF;
               END RET_T1;

               FUNCTION RET_CHAR (X : CHARACTER) RETURN T10 IS
               BEGIN
                    RETURN T10(X);
               END RET_CHAR;

               PROCEDURE DO_NOTHING (X : IN OUT T3) IS
               BEGIN
                    IF X = TRUE THEN
                         X := FALSE;
                    ELSE
                         X := TRUE;
                    END IF;
               END DO_NOTHING;
          END BPACK;
     END APACK;

     PROCEDURE NEW_DO_NOTHING IS NEW APACK.BPACK.DO_NOTHING;

     BEGIN

          -- A1: VISIBILITY FOR UNOVERLOADED ENUMERATION LITERALS

          IF  APACK.BPACK.">"(APACK.BPACK.RED, APACK.BPACK.GREEN) THEN
               FAILED ("VISIBILITY FOR UNOVERLOADED ENUMERATION " &
                       "LITERAL BAD - A1");
          END IF;


          -- A2: VISIBILITY FOR OVERLOADED
          --     ENUMERATION CHARACTER LITERALS

          IF  APACK.BPACK."<"(APACK.BPACK.T2A'(APACK.BPACK.'C'),
                              APACK.BPACK.T2A'(APACK.BPACK.'B')) THEN
               FAILED ("VISIBILITY FOR OVERLOADED ENUMERATION " &
                       "LITERAL BAD - A2");
          END IF;


          -- A3: VISIBILITY FOR A DERIVED BOOLEAN TYPE

          IF APACK.BPACK."<"(APACK.BPACK.T3'(APACK.BPACK.TRUE),
                             APACK.BPACK.FALSE) THEN
               FAILED ("VISIBILITY FOR DERIVED BOOLEAN BAD - A3");
          END IF;


          -- A4: VISIBILITY FOR AN INTEGER TYPE

          IF APACK.BPACK."/="(APACK.BPACK."MOD"(6,2),APACK.BPACK.ZERO)
               THEN FAILED ("VISIBILITY FOR INTEGER TYPE BAD - A4");
          END IF;


          -- A5: VISIBILITY FOR A FLOATING POINT TYPE

          IF APACK.BPACK.">"(APACK.BPACK.T5'(2.7),APACK.BPACK.A_FLT)
               THEN FAILED ("VISIBILITY FOR FLOATING POINT BAD - A5");
          END IF;


          -- A6: VISIBILITY FOR A FIXED POINT INVOLVING UNARY MINUS

          IF APACK.BPACK."<"(APACK.BPACK.A_FIX,APACK.BPACK.T67'
                            (APACK.BPACK."-"(1.5))) THEN
               FAILED ("VISIBILITY FOR FIXED POINT WITH UNARY MINUS " &
                       "BAD - A6");
          END IF;


          -- A7: VISIBILITY FOR A FIXED POINT DIVIDED BY INTEGER

          IF APACK.BPACK."/="(APACK.BPACK.T67(-0.5),APACK.BPACK."/"
                             (APACK.BPACK.A_FIX,2)) THEN
               FAILED ("VISIBILITY FOR FIXED POINT DIVIDED BY " &
                       "INTEGER BAD - A7");
          END IF;


          -- A8: VISIBILITY FOR ARRAY EQUALITY

          IF APACK.BPACK."/="(APACK.BPACK.ARY,(APACK.BPACK.T3(TRUE),
             APACK.BPACK.T3(TRUE),APACK.BPACK.T3(TRUE),
             APACK.BPACK.T3(FALSE))) THEN
               FAILED ("VISIBILITY FOR ARRAY EQUALITY BAD - A8");
          END IF;


          -- A9: VISIBILITY FOR ACCESS EQUALITY

          IF APACK.BPACK."/="(APACK.BPACK.P1(3),
                              APACK.BPACK.T3(IDENT_BOOL(TRUE)))
               THEN FAILED ("VISIBILITY FOR ACCESS EQUALITY BAD - A9");
          END IF;


          -- A10: VISIBILITY FOR PRIVATE TYPE

          IF APACK.BPACK."/="(APACK.BPACK.C1,
                              APACK.BPACK.RET_CHAR('J')) THEN
               FAILED ("VISIBILITY FOR PRIVATE TYPE BAD - A10");
          END IF;


          -- A11: VISIBILITY FOR DERIVED SUBPROGRAM

          IF APACK.BPACK."/="(APACK.BPACK.RET_T1(APACK.BPACK.RED),
                              APACK.BPACK.GREEN) THEN
               FAILED ("VISIBILITY FOR DERIVED SUBPROGRAM BAD - A11");
          END IF;

          -- A12: VISIBILITY FOR GENERIC SUBPROGRAM

          NEW_DO_NOTHING (APACK.BPACK.V1);

          IF APACK.BPACK."/="(APACK.BPACK.V1,APACK.BPACK.T3(TRUE)) THEN
               FAILED ("VISIBILITY FOR GENERIC SUBPROGRAM BAD - A12");
          END IF;

     END A_BLOCK;

     B_BLOCK:
     DECLARE
          GENERIC
               TYPE T1 IS (<>);
          PACKAGE GENPACK IS
               PACKAGE APACK IS
                    PACKAGE BPACK  IS
                         TYPE    T1  IS (ORANGE,GREEN);
                         TYPE    T2A IS ('E', 'F', 'G');
                         TYPE    T3  IS NEW BOOLEAN;
                         TYPE    T4  IS NEW INTEGER RANGE -3 .. 8;
                         TYPE    T5  IS DIGITS 5;
                         TYPE    T67 IS DELTA 0.5 RANGE -3.0 .. 25.0;
                         TYPE    T9A IS ARRAY (INTEGER RANGE <>) OF T3;
                         SUBTYPE T9B IS T9A (2 .. 8);
                         TYPE    T9C IS ACCESS T9B;
                         TYPE    T10 IS PRIVATE;
                         V1    : T3 := TRUE;
                         SIX   : T4 := 6;
                         B_FLT : T5 := 4.0;
                         ARY   : T9A(1..4) := (TRUE,FALSE,TRUE,FALSE);
                         P1    : T9C := NEW T9B'( 2..4 => T3'(FALSE),
                                                  5..8 => T3'(TRUE));
                         K1 : CONSTANT T10;

                         FUNCTION RET_T1 (X : T1) RETURN T1;

                         FUNCTION RET_CHAR (X : CHARACTER) RETURN T10;

                         GENERIC
                         PROCEDURE DO_NOTHING (X : IN OUT T3);
                    PRIVATE
                         TYPE T10 IS NEW CHARACTER;
                         K1 : CONSTANT T10 := 'V';
                    END BPACK;
               END APACK;
          END GENPACK;

          PACKAGE BODY GENPACK IS
               PACKAGE BODY APACK IS
                    PACKAGE BODY BPACK IS
                         FUNCTION RET_T1 (X : T1) RETURN T1 IS
                         BEGIN
                              IF X = ORANGE THEN
                                   RETURN GREEN;
                              ELSE
                                   RETURN ORANGE;
                              END IF;
                         END RET_T1;

                         FUNCTION RET_CHAR (X : CHARACTER) RETURN T10 IS
                         BEGIN
                              RETURN T10(X);
                         END RET_CHAR;

                         PROCEDURE DO_NOTHING (X : IN OUT T3) IS
                         BEGIN
                              IF X = TRUE THEN
                                   X := FALSE;
                              ELSE
                                   X := TRUE;
                              END IF;
                         END DO_NOTHING;
                    END BPACK;
               END APACK;
          END GENPACK;

          PACKAGE MYPACK IS NEW GENPACK (T1 => INTEGER);

          PROCEDURE MY_DO_NOTHING IS NEW MYPACK.APACK.BPACK.DO_NOTHING;

     BEGIN

          -- B1: GENERIC INSTANCE OF UNOVERLOADED ENUMERATION LITERAL

          IF MYPACK.APACK.BPACK."<"(MYPACK.APACK.BPACK.GREEN,
                                    MYPACK.APACK.BPACK.ORANGE) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF " &
                       "UNOVERLOADED ENUMERATION LITERAL BAD - B1");
          END IF;


          -- B2: GENERIC INSTANCE OF OVERLOADED ENUMERATION LITERAL

          IF  MYPACK.APACK.BPACK.">"(MYPACK.APACK.BPACK.T2A'(MYPACK.
             APACK.BPACK.'F'),MYPACK.APACK.BPACK.T2A'(MYPACK.APACK.
             BPACK.'G')) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF " &
                       "OVERLOADED ENUMERATION LITERAL BAD - B2");
          END IF;


          -- B3: VISIBILITY FOR GENERIC INSTANCE OF DERIVED BOOLEAN

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK."NOT"(MYPACK.
             APACK.BPACK.T3'(MYPACK.APACK.BPACK.TRUE)),MYPACK.APACK.
             BPACK.FALSE) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF DERIVED " &
                       "BOOLEAN BAD - B3");
          END IF;


          -- B4: VISIBILITY FOR GENERIC INSTANCE OF DERIVED INTEGER

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK."MOD"(MYPACK.
             APACK.BPACK.SIX,2),0) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF INTEGER " &
                       "BAD - B4");
          END IF;


          -- B5: VISIBILITY FOR GENERIC INSTANCE OF FLOATING POINT

          IF MYPACK.APACK.BPACK.">"(MYPACK.APACK.BPACK.T5'(1.9),MYPACK.
             APACK.BPACK.B_FLT) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF FLOATING " &
                       "POINT BAD - B5");
          END IF;


          -- B6: VISIBILITY FOR GENERIC INSTANCE OF
          --     FIXED POINT UNARY PLUS

          IF MYPACK.APACK.BPACK."<"(2.5,MYPACK.APACK.BPACK.T67'(MYPACK.
             APACK.BPACK."+"(1.75))) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF FIXED " &
                       "POINT UNARY PLUS BAD - B6");
          END IF;


          -- B7: VISIBILITY FOR GENERIC INSTANCE OF
          --     FIXED POINT DIVIDED BY INTEGER

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK."/"(2.5,4),
             0.625) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF FIXED " &
                       "POINT DIVIDED BY INTEGER BAD - B7");
          END IF;


          -- B8: VISIBILITY FOR GENERIC INSTANCE OF ARRAY EQUALITY

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK.ARY,(MYPACK.
             APACK.BPACK.T3(TRUE),MYPACK.APACK.BPACK.T3(FALSE),MYPACK.
             APACK.BPACK.T3(TRUE),MYPACK.APACK.BPACK.T3(FALSE))) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF ARRAY " &
                       "EQUALITY BAD - B8");
          END IF;


          -- B9: VISIBILITY FOR GENERIC INSTANCE OF ACCESS EQUALITY

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK.P1(3),MYPACK.
             APACK.BPACK.T3(IDENT_BOOL(FALSE))) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF ACCESS " &
                       "EQUALITY BAD - B9");
          END IF;


          -- B10: VISIBILITY FOR GENERIC INSTANCE OF PRIVATE EQUALITY

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK.K1,MYPACK.APACK.
             BPACK.RET_CHAR('V')) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF PRIVATE " &
                       "EQUALITY BAD - B10");
          END IF;


          -- B11: VISIBILITY FOR GENERIC INSTANCE OF DERIVED SUBPROGRAM

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK.RET_T1(MYPACK.
             APACK.BPACK.ORANGE),MYPACK.APACK.BPACK.GREEN) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF DERIVED " &
                       "SUBPROGRAM BAD - B11");
          END IF;

          -- B12: VISIBILITY FOR GENERIC INSTANCE OF GENERIC SUBPROGRAM

          MY_DO_NOTHING (MYPACK.APACK.BPACK.V1);

          IF MYPACK.APACK.BPACK."/="(MYPACK.APACK.BPACK.V1,
                                     MYPACK.APACK.BPACK.T3(FALSE)) THEN
               FAILED ("VISIBILITY FOR GENERIC INSTANCE OF GENERIC " &
                       "SUBPROGRAM BAD - B12");
          END IF;

     END B_BLOCK;

     RESULT;
END C83051A;
