-- CC1304A.ADA

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
-- CHECK THAT GENERIC FORMAL SUBPROGRAMS MAY HAVE A PARAMETER
-- OF A GENERIC FORMAL TYPE, AND MAY RETURN A GENERIC FORMAL
-- TYPE.

-- DAT 8/27/81

WITH REPORT; USE REPORT;

PROCEDURE CC1304A IS
BEGIN
     TEST ("CC1304A", "GENERIC FORMAL SUBPROGRAMS MAY HAVE PARAMETERS"
          & " OF (AND RETURN) A FORMAL TYPE");

     DECLARE
          GENERIC
               TYPE T IS ( <> );
               WITH FUNCTION S (P : T) RETURN T;
               WITH PROCEDURE P (P : T);
          PROCEDURE PR (PARM : T);

          PROCEDURE PR (PARM: T) IS
          BEGIN
               P(P=>S(P=>PARM));
          END PR;
     BEGIN
          DECLARE
               C : CHARACTER := 'A';
               B : BOOLEAN := FALSE;
               I : INTEGER := 5;
               TYPE ENUM IS (E1, E2, E3);
               E : ENUM := E2;

               FUNCTION FC (P : CHARACTER) RETURN CHARACTER IS
               BEGIN
                    RETURN 'B';
               END FC;

               FUNCTION FB (P : BOOLEAN) RETURN BOOLEAN IS
               BEGIN
                    RETURN NOT P;
               END FB;
     
               FUNCTION FI (P : INTEGER) RETURN INTEGER IS
               BEGIN
                    RETURN P + 1;
               END FI;

               FUNCTION FE (P : ENUM) RETURN ENUM IS
               BEGIN
                    RETURN ENUM'SUCC (P);
               END FE;

               PROCEDURE  PC (P : CHARACTER) IS
               BEGIN
                    C := P;
               END PC;

               PROCEDURE PB (P : BOOLEAN) IS
               BEGIN
                    B := P;
               END PB;

               PROCEDURE PI (P : INTEGER) IS
               BEGIN
                   I := P;
               END PI;
 
               PROCEDURE PE (P : ENUM) IS
               BEGIN
                    E := P;
               END PE;

               PACKAGE PKG2 IS
                    PROCEDURE P1 IS NEW PR (CHARACTER, FC, PC);
                    PROCEDURE P2 IS NEW PR (BOOLEAN, FB, PB);
                    PROCEDURE P3 IS NEW PR (INTEGER, FI, PI);
                    PROCEDURE P4 IS NEW PR (ENUM, FE, PE);
               END PKG2;

               PACKAGE BODY PKG2 IS
               BEGIN
                    P1 (C);
                    P2 (B);
                    P3 (I);
                    P4 (E);
               END  PKG2;
          BEGIN
               IF C /= 'B'
               OR B /= TRUE
               OR I /= 6
               OR E /= E3 THEN
                    FAILED ("SUBPROGRAM PARAMETERS OF FORMAL TYPES");
               END IF;
          END;
     END;
     
     RESULT;
END CC1304A;
