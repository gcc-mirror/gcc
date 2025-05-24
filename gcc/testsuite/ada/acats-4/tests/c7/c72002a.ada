-- C72002A.ADA

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
--     CHECK THAT THE DECLARATIVE ITEMS IN A PACKAGE SPECIFICATION ARE
--     ELABORATED IN THE ORDER DECLARED.

-- HISTORY:
--     DHH 03/09/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C72002A IS

     A : INTEGER := 0;
     TYPE ORDER_ARRAY IS ARRAY(1 .. 14) OF INTEGER;
     OBJECT_ARRAY : ORDER_ARRAY;
     TYPE REAL IS DIGITS 4;
     TYPE ENUM IS (RED,YELLOW,BLUE);

     TYPE ARR IS ARRAY(1 ..2) OF BOOLEAN;
     D : ARR := (TRUE, TRUE);
     E : ARR := (FALSE, FALSE);

     TYPE REC IS
          RECORD
               I : INTEGER;
          END RECORD;
     B : REC := (I => IDENT_INT(1));
     C : REC := (I => IDENT_INT(2));

     FUNCTION GIVEN_ORDER(X : INTEGER) RETURN INTEGER IS
          Y : INTEGER;
     BEGIN
          Y := X + 1;
          RETURN Y;
     END GIVEN_ORDER;

     FUNCTION BOOL(X : INTEGER) RETURN BOOLEAN IS
     BEGIN
          IF X = IDENT_INT(1) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN TRUE;
          ELSIF X = IDENT_INT(8) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN FALSE;
          END IF;
     END BOOL;

     FUNCTION INT(X : INTEGER) RETURN INTEGER IS
     BEGIN
          IF X = IDENT_INT(2) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN IDENT_INT(1);
          ELSIF X = IDENT_INT(9) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN IDENT_INT(2);
          END IF;
     END INT;

     FUNCTION FLOAT(X : INTEGER) RETURN REAL IS
     BEGIN
          IF X = IDENT_INT(3) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN 1.0;
          ELSIF X = IDENT_INT(10) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN 2.0;
          END IF;
     END FLOAT;

     FUNCTION CHAR(X : INTEGER) RETURN CHARACTER IS
     BEGIN
          IF X = IDENT_INT(4) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN 'A';
          ELSIF X = IDENT_INT(11) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN 'Z';
          END IF;
     END CHAR;

     FUNCTION ENUMR(X : INTEGER) RETURN ENUM IS
     BEGIN
          IF X = IDENT_INT(5) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN RED;
          ELSIF X = IDENT_INT(12) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN YELLOW;
          END IF;
     END ENUMR;

     FUNCTION ARRY(X : INTEGER) RETURN ARR IS
     BEGIN
          IF X = IDENT_INT(6) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN D;
          ELSIF X = IDENT_INT(13) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN E;
          END IF;
     END ARRY;

     FUNCTION RECOR(X : INTEGER) RETURN REC IS
     BEGIN
          IF X = IDENT_INT(7) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN B;
          ELSIF X = IDENT_INT(14) THEN
               A := GIVEN_ORDER(A);
               OBJECT_ARRAY(X) := A;
               RETURN C;
          END IF;
     END RECOR;

     PACKAGE PACK IS
          A : BOOLEAN   := BOOL(1);
          B : INTEGER   := INT(2);
          C : REAL      := FLOAT(3);
          D : CHARACTER := CHAR(4);
          E : ENUM      := ENUMR(5);
          F : ARR       := ARRY(6);
          G : REC       := RECOR(7);
          H : BOOLEAN   := BOOL(8);
          I : INTEGER   := INT(9);
          J : REAL      := FLOAT(10);
          K : CHARACTER := CHAR(11);
          L : ENUM      := ENUMR(12);
          M : ARR       := ARRY(13);
          N : REC       := RECOR(14);
     END PACK;

BEGIN
     TEST("C72002A", "CHECK THAT THE DECLARATIVE ITEMS IN A PACKAGE " &
                     "SPECIFICATION ARE ELABORATED IN THE ORDER " &
                     "DECLARED");

     IF OBJECT_ARRAY(1) /= IDENT_INT(1) THEN
          FAILED("BOOLEAN 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(2) /= IDENT_INT(2) THEN
          FAILED("INTEGER 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(3) /= IDENT_INT(3) THEN
          FAILED("REAL 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(4) /= IDENT_INT(4) THEN
          FAILED("CHARACTER 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(5) /= IDENT_INT(5) THEN
          FAILED("ENUMERATION 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(6) /= IDENT_INT(6) THEN
          FAILED("ARRAY 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(7) /= IDENT_INT(7) THEN
          FAILED("RECORD 1 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(8) /= IDENT_INT(8) THEN
          FAILED("BOOLEAN 2 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(9) /= IDENT_INT(9) THEN
          FAILED("INTEGER 2 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(10) /= IDENT_INT(10) THEN
          FAILED("REAL 2 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(11) /= IDENT_INT(11) THEN
          FAILED("CHARACTER 2 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(12) /= IDENT_INT(12) THEN
          FAILED("ENUMERATION 2 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(13) /= IDENT_INT(13) THEN
          FAILED("ARRAY 2 ELABORATED OUT OF ORDER");
     END IF;

     IF OBJECT_ARRAY(14) /= IDENT_INT(14) THEN
          FAILED("RECORD 2 ELABORATED OUT OF ORDER");
     END IF;

     RESULT;
END C72002A;
