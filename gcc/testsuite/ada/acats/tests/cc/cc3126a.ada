-- CC3126A.ADA

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
--     CHECK THAT CONSTRAINT_ERROR IS RAISED IF AND ONLY IF THE ACTUAL
--     PARAMETER DOES NOT HAVE THE SAME NUMBER OF COMPONENTS
--     (PER DIMENSION) AS THE FORMAL PARAMETER. ALSO THAT FOR NULL
--     ARRAYS NO ERROR IS RAISED.

-- HISTORY:
--     LB  12/02/86
--     DWC 08/11/87  CHANGED HEADING FORMAT.
--     RJW 10/26/89  INITIALIZED VARIABLE H.

WITH REPORT; USE REPORT;

PROCEDURE  CC3126A  IS

BEGIN
     TEST ("CC3126A","GENERIC ACTUAL PARAMETER MUST HAVE THE SAME "&
                     "NUMBER OF COMPONENTS (PER DIMENSION) AS THE "&
                     "GENERIC FORMAL PARMETER");
     BEGIN
          DECLARE
               TYPE ARRY1 IS ARRAY (INTEGER RANGE <>) OF INTEGER;
               SUBTYPE ARR IS ARRY1 (1 .. 10);

               GENERIC
                    GARR : IN ARR;
               PACKAGE P IS
                    NARR : ARR := GARR;
               END P;

          BEGIN
               BEGIN
                    DECLARE
                         X : ARRY1 (2 .. 11) := (2 .. 11 => 0);
                         PACKAGE Q IS NEW P(X);
                    BEGIN
                         Q.NARR(2) := 1;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED 1");
               END;

               BEGIN
                    DECLARE
                         S : ARRY1 (1 .. 11) := (1 .. 11 => 0);
                         PACKAGE R IS NEW P(S);
                    BEGIN
                         FAILED ("EXCEPTION NOT RAISED 2");
                         R.NARR(1) := IDENT_INT(R.NARR(1));
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED 2");
               END;

               BEGIN
                    DECLARE
                         G : ARRY1 (1 .. 9) := (1 .. 9 => 0);
                         PACKAGE K IS NEW P(G);
                    BEGIN
                         FAILED ("EXCEPTION NOT RAISED 3");
                         IF EQUAL(3,3) THEN
                              K.NARR(1) := IDENT_INT(K.NARR(1));
                         END IF;
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED 3");
               END;

               BEGIN
                    DECLARE
                         S : ARRY1 (1 .. 11) := (1 .. 11 => 0);
                         PACKAGE F IS NEW P(S(2 .. 11));
                    BEGIN
                         F.NARR(2) := IDENT_INT(F.NARR(2));
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED 4");
               END;
          END;

          DECLARE
               SUBTYPE STR IS STRING(1 .. 20);

               GENERIC
                    GVAR : IN STR;
               PACKAGE M IS
                    NVAR : STR := GVAR;
               END M;

          BEGIN
               BEGIN
                    DECLARE
                         L : STRING (2 .. 15);
                         PACKAGE U IS NEW M(L);
                    BEGIN
                         FAILED ("EXCEPTION NOT RAISED 5");
                         U.NVAR(2) := IDENT_CHAR(U.NVAR(2));
                    END;
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("WRONG EXCEPTION RAISED 5");
               END;

               BEGIN
                    DECLARE
                         H : STRING (1 .. 20) := (OTHERS => 'R');
                         PACKAGE J IS NEW M(H);
                    BEGIN
                         IF EQUAL(3,3) THEN
                              J.NVAR(2) := IDENT_CHAR(J.NVAR(2));
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED 6");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED ERROR RAISED STRINGS");
          END;

          DECLARE
               TYPE NARRY IS ARRAY (INTEGER RANGE <>) OF INTEGER;
               SUBTYPE SNARRY IS NARRY (2 .. 0);

               GENERIC
                    RD : IN SNARRY;
               PACKAGE JA IS
                    CD : SNARRY := RD;
               END JA;
          BEGIN
               BEGIN
                    DECLARE
                         AD : NARRY(1 .. 0);
                         PACKAGE PA IS NEW JA(AD);
                    BEGIN
                         IF NOT EQUAL(0,PA.CD'LAST) THEN
                              FAILED ("PARAMETER ATTRIBUTE INCORRECT");
                         END IF;
                    END;
               EXCEPTION
                    WHEN OTHERS =>
                         FAILED ("EXCEPTION RAISED 7");
               END;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED FOR ARRAYS "&
                            "WITH NULL RANGES");
          END;
     END;

     RESULT;

END CC3126A;
