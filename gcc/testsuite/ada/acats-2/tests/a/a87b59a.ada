-- A87B59A.ADA

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
-- CHECK THAT BECAUSE A GENERIC ACTUAL PROGRAM PARAMETER MUST BE A 
-- SUBPROGRAM, AN ENUMERATION LITERAL, OR AN ENTRY WITH THE SAME 
-- PARAMETER AND RESULT TYPE PROFILE AS THE FORMAL PARAMETER, AN 
-- OVERLOADED NAME APPEARING AS AN ACTUAL PARAMETER CAN BE RESOLVED.

-- R.WILLIAMS 9/24/86

WITH REPORT; USE REPORT;
PROCEDURE A87B59A IS

BEGIN
     TEST ( "A87B59A", "CHECK THAT BECAUSE A GENERIC ACTUAL PROGRAM " &
                       "PARAMETER MUST BE A SUBPROGRAM, AN " &
                       "ENUMERATION LITERAL, OR AN ENTRY WITH THE " &
                       "SAME PARAMETER AND RESULT TYPE PROFILE AS " &
                       "THE FORMAL PARAMETER, AN OVERLOADED NAME " &
                       "APPEARING AS AN ACTUAL PARAMETER CAN BE " &
                       "RESOLVED" );

     DECLARE -- A.
          FUNCTION F1 RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (0);
          END F1;

          FUNCTION F1 RETURN BOOLEAN IS
          BEGIN
               RETURN IDENT_BOOL (TRUE);
          END F1;

          GENERIC
               TYPE T IS (<>);
               WITH FUNCTION F RETURN T;
          PROCEDURE P;

          PROCEDURE P IS
          BEGIN
               NULL;
          END P;

          PROCEDURE P1 IS NEW P (INTEGER, F1);
          PROCEDURE P2 IS NEW P (BOOLEAN, F1);

     BEGIN         
          P1;
          P2;
     END; -- A.
          
     DECLARE -- B.
          FUNCTION F1 (X : INTEGER; B : BOOLEAN) RETURN INTEGER IS
          BEGIN
               RETURN IDENT_INT (X);
          END F1;

          FUNCTION F1 (X : INTEGER; B : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN IDENT_BOOL (B);
          END F1;

          FUNCTION F1 (B : BOOLEAN; X : INTEGER) RETURN BOOLEAN IS
          BEGIN
               RETURN IDENT_BOOL (B);
          END F1;
          
          GENERIC
               TYPE T1 IS (<>);
               TYPE T2 IS (<>);
               WITH FUNCTION F (A : T1; B : T2) RETURN T1;
          PROCEDURE P1;

          PROCEDURE P1 IS
          BEGIN
               NULL;
          END P1;

          GENERIC
               TYPE T1 IS (<>);
               TYPE T2 IS (<>);
               WITH FUNCTION F (A : T1; B : T2) RETURN T2;
          PROCEDURE P2;

          PROCEDURE P2 IS
          BEGIN
               NULL;
          END P2;

          PROCEDURE PROC1 IS NEW P1 (INTEGER, BOOLEAN, F1);
          PROCEDURE PROC2 IS NEW P1 (BOOLEAN, INTEGER, F1);
          PROCEDURE PROC3 IS NEW P2 (INTEGER, BOOLEAN, F1);

     BEGIN         
          PROC1;
          PROC2;
     END; -- B.

     DECLARE -- C.
          TYPE COLOR IS (RED, YELLOW, BLUE);
          C : COLOR;

          TYPE LIGHT IS (RED, YELLOW, GREEN);          
          L : LIGHT;

          GENERIC
               TYPE T IS (<>);
               WITH FUNCTION F RETURN T;
          FUNCTION GF RETURN T;

          FUNCTION GF RETURN T IS
          BEGIN
               RETURN T'VAL (IDENT_INT (T'POS (F)));
          END GF;

          FUNCTION F1 IS NEW GF (COLOR, RED);
          FUNCTION F2 IS NEW GF (LIGHT, YELLOW);
     BEGIN
          C := F1;
          L := F2;
     END; -- C.

     DECLARE -- D.
          TASK TK IS
               ENTRY E (X : INTEGER);
               ENTRY E (X : BOOLEAN);
               ENTRY E (X : INTEGER; Y : BOOLEAN);
               ENTRY E (X : BOOLEAN; Y : INTEGER);
          END TK;

          TASK BODY TK IS
          BEGIN
               LOOP
                    SELECT
                         ACCEPT E (X : INTEGER);
                    OR
                         ACCEPT E (X : BOOLEAN);
                    OR 
                         ACCEPT E (X : INTEGER; Y : BOOLEAN);
                    OR 
                         ACCEPT E (X : BOOLEAN; Y : INTEGER);
                    OR 
                         TERMINATE;
                    END SELECT;
               END LOOP;
          END TK;

          GENERIC
               TYPE T1 IS (<>);
               TYPE T2 IS (<>);
               WITH PROCEDURE P1 (X : T1);
               WITH PROCEDURE P2 (X : T1; Y : T2);
          PACKAGE PKG IS 
               PROCEDURE P;
          END PKG;

          PACKAGE BODY PKG IS
               PROCEDURE P IS
               BEGIN
                    IF EQUAL (3, 3) THEN
                         P1 (T1'VAL (1));
                         P2 (T1'VAL (0), T2'VAL (1));
                    END IF;
               END P;
          END PKG;

          PACKAGE PK1 IS NEW PKG (INTEGER, BOOLEAN, TK.E, TK.E);
          PACKAGE PK2 IS NEW PKG (BOOLEAN, INTEGER, TK.E, TK.E);

     BEGIN
          PK1.P;
          PK2.P;               
     END; -- D.

     DECLARE -- E.
          FUNCTION "+" (X, Y : BOOLEAN) RETURN BOOLEAN IS
          BEGIN
               RETURN IDENT_BOOL (X OR Y);
          END "+";

          GENERIC
               TYPE T IS (<>);
               WITH FUNCTION "+" (X, Y : T) RETURN T;
          PROCEDURE P;

          PROCEDURE P IS
               S : T;
          BEGIN
               S := "+" (T'VAL (0), T'VAL (1));
          END P;

          PROCEDURE P1 IS NEW P (BOOLEAN, "+");
          PROCEDURE P2 IS NEW P (INTEGER, "+");
     
     BEGIN
          P1;
          P2;
     END; -- E.
          
     DECLARE -- F.
          TYPE ADD_OPS IS ('+', '-', '&');

          GENERIC
               TYPE T1 IS (<>);
               TYPE T2 IS (<>);
               TYPE T3 IS ARRAY (POSITIVE RANGE <> ) OF T2;
               X2 : T2;
               X3 : T3;
               WITH FUNCTION F1 RETURN T1;
               WITH FUNCTION F2 (X : T2; Y : T3) RETURN T3;
          PROCEDURE P;

          PROCEDURE P IS
               A : T1;
               S : T3 (IDENT_INT (1) .. IDENT_INT (2));               
          BEGIN
               A := F1;
               S := F2 (X2, X3);               
          END P;
          
          PROCEDURE P1 IS NEW P (ADD_OPS, CHARACTER, STRING,
                                 '&', "&", '&', "&");
     
     BEGIN
          P1;
     END; -- F.
          
     RESULT;
END A87B59A;
