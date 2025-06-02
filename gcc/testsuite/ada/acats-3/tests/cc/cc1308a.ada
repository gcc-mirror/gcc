-- CC1308A.ADA

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
--     CHECK THAT FORMAL SUBPROGRAM PARAMETERS MAY OVERLOAD EACH OTHER
--     AND OTHER VISIBLE SUBPROGRAMS AND ENUMERATION LITERALS WITHIN AND
--     OUTSIDE OF THE GENERIC UNIT.

-- HISTORY:
--     DAT 09/08/81  CREATED ORIGINAL TEST.
--     SPS 10/26/82
--     SPS 02/09/83
--     BCB 08/09/88  REPLACED THE OLD TEST WITH A VERSION BASED ON
--                   AIG 6.6/T2.

WITH REPORT; USE REPORT;

PROCEDURE CC1308A IS

     TYPE ENUM IS (F1,F2,F3,F4,F5,F6,F7);

     FUNCTION F1 (X : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN 2*X;
     END F1;

     PROCEDURE F1 (X : IN OUT INTEGER) IS
     BEGIN
          X := 3*X;
     END F1;

     PROCEDURE F2 (Y : IN OUT INTEGER; Z : IN OUT BOOLEAN) IS
     BEGIN
          Y := 2*Y;
          Z := NOT Z;
     END F2;

     PROCEDURE F2 (Y : IN OUT INTEGER) IS
     BEGIN
          Y := 3*Y;
     END F2;

     PROCEDURE F3 (B : BOOLEAN := FALSE; A : IN OUT INTEGER) IS
     BEGIN
          A := 2*A;
     END F3;

     PROCEDURE F3 (A : IN OUT INTEGER) IS
     BEGIN
          A := 3*A;
     END F3;

     PROCEDURE F4 (C : IN OUT INTEGER) IS
     BEGIN
          C := 2*C;
     END F4;

     PROCEDURE F4 (C : IN OUT BOOLEAN) IS
     BEGIN
          C := NOT C;
     END F4;

     PROCEDURE F5 (D : IN OUT INTEGER; E : IN OUT BOOLEAN) IS
     BEGIN
          D := 2*D;
          E := NOT E;
     END F5;

     PROCEDURE F5 (E : IN OUT BOOLEAN; D : IN OUT INTEGER) IS
     BEGIN
          E := NOT E;
          D := 3*D;
     END F5;

     FUNCTION F6 (G : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN 2*G;
     END F6;

     FUNCTION F6 (G : INTEGER) RETURN BOOLEAN IS
     BEGIN
          RETURN TRUE;
     END F6;

     FUNCTION F7 RETURN INTEGER IS
     BEGIN
          RETURN 25;
     END F7;

     FUNCTION F7 RETURN BOOLEAN IS
     BEGIN
          RETURN FALSE;
     END F7;

BEGIN
     TEST ("CC1308A", "CHECK THAT FORMAL SUBPROGRAM PARAMETERS MAY " &
                      "OVERLOAD EACH OTHER AND OTHER VISIBLE " &
                      "SUBPROGRAMS AND ENUMERATION LITERALS WITHIN " &
                      "AND OUTSIDE OF THE GENERIC UNIT");

     DECLARE
          GENERIC
               WITH FUNCTION F1 (X : INTEGER) RETURN INTEGER;
               WITH PROCEDURE F1 (X : IN OUT INTEGER);

               WITH PROCEDURE F2 (Y : IN OUT INTEGER;
                                  Z : IN OUT BOOLEAN);
               WITH PROCEDURE F2 (Y : IN OUT INTEGER);

               WITH PROCEDURE F3 (B : BOOLEAN := FALSE;
                                  A : IN OUT INTEGER);
               WITH PROCEDURE F3 (A : IN OUT INTEGER);

               WITH PROCEDURE F4 (C : IN OUT INTEGER);
               WITH PROCEDURE F4 (C : IN OUT BOOLEAN);

               WITH PROCEDURE F5 (D : IN OUT INTEGER;
                                  E : IN OUT BOOLEAN);
               WITH PROCEDURE F5 (E : IN OUT BOOLEAN;
                                  D : IN OUT INTEGER);

               WITH FUNCTION F6 (G : INTEGER) RETURN INTEGER;
               WITH FUNCTION F6 (G : INTEGER) RETURN BOOLEAN;

               WITH FUNCTION F7 RETURN INTEGER;
               WITH FUNCTION F7 RETURN BOOLEAN;
          PACKAGE P IS
               TYPE EN IS (F1,F2,F3,F4,F5,F6,F7);
          END P;

          PACKAGE BODY P IS
               X1, X2, Y1, Y2, A1, A2, C1, D1, D2, G1
                  : INTEGER := IDENT_INT(5);

               VAL : INTEGER := IDENT_INT(0);

               Z1, B1, C2, E1, E2, BOOL : BOOLEAN := IDENT_BOOL(FALSE);
          BEGIN
               VAL := F1(X1);

               IF NOT EQUAL(VAL,10) THEN
                    FAILED ("CASE 1 - WRONG VALUE RETURNED FROM " &
                            "FUNCTION");
               END IF;

               F1(X2);

               IF NOT EQUAL(X2,15) THEN
                    FAILED ("CASE 1 - WRONG VALUE ASSIGNED INSIDE " &
                            "PROCEDURE");
               END IF;

               F2(Y1,Z1);

               IF NOT EQUAL(Y1,10) OR Z1 /= TRUE THEN
                    FAILED ("CASE 2 - WRONG VALUES ASSIGNED INSIDE " &
                            "PROCEDURE");
               END IF;

               F2(Y2);

               IF NOT EQUAL(Y2,15) THEN
                    FAILED ("CASE 2 - WRONG VALUE ASSIGNED INSIDE " &
                            "PROCEDURE");
               END IF;

               F3(B1,A1);

               IF NOT EQUAL(A1,10) OR B1 /= FALSE THEN
                    FAILED ("CASE 3 - WRONG VALUES ASSIGNED INSIDE " &
                            "PROCEDURE");
               END IF;

               F3(A2);

               IF NOT EQUAL(A2,15) THEN
                    FAILED ("CASE 3 - WRONG VALUE ASSIGNED INSIDE " &
                            "PROCEDURE");
               END IF;

               F4(C1);

               IF NOT EQUAL(C1,10) THEN
                    FAILED ("CASE 4 - WRONG VALUE ASSIGNED INSIDE " &
                            "PROCEDURE - BASE TYPE INTEGER");
               END IF;

               F4(C2);

               IF C2 /= TRUE THEN
                    FAILED ("CASE 4 - WRONG VALUE ASSIGNED INSIDE " &
                            "PROCEDURE - BASE TYPE BOOLEAN");
               END IF;

               F5(D1,E1);

               IF NOT EQUAL(D1,10) OR E1 /= TRUE THEN
                    FAILED ("CASE 5 - WRONG VALUES ASSIGNED INSIDE " &
                            "PROCEDURE - ORDER WAS INTEGER, BOOLEAN");
               END IF;

               F5(E2,D2);

               IF E2 /= TRUE OR NOT EQUAL(D2,15) THEN
                    FAILED ("CASE 5 - WRONG VALUES ASSIGNED INSIDE " &
                            "PROCEDURE - ORDER WAS BOOLEAN, INTEGER");
               END IF;

               VAL := F6(G1);

               IF NOT EQUAL(VAL,10) THEN
                    FAILED ("CASE 6 - WRONG VALUE RETURNED FROM " &
                            "FUNCTION - TYPE INTEGER");
               END IF;

               BOOL := F6(G1);

               IF BOOL /= TRUE THEN
                    FAILED ("CASE 6 - WRONG VALUE RETURNED FROM " &
                            "FUNCTION - TYPE BOOLEAN");
               END IF;

               VAL := F7;

               IF NOT EQUAL(VAL,25) THEN
                    FAILED ("CASE 7 - WRONG VALUE RETURNED FROM " &
                            "PARAMETERLESS FUNCTION - TYPE INTEGER");
               END IF;

               BOOL := F7;

               IF BOOL /= FALSE THEN
                    FAILED ("CASE 7 - WRONG VALUE RETURNED FROM " &
                            "PARAMETERLESS FUNCTION - TYPE BOOLEAN");
               END IF;
          END P;

          PACKAGE NEW_P IS NEW P (F1, F1, F2, F2, F3, F3,
                                  F4, F4, F5, F5, F6, F6, F7, F7);
     BEGIN
          NULL;
     END;

     RESULT;
END CC1308A;
