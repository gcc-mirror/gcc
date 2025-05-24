-- CC1304B.ADA

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
--     CHECK THAT GENERIC FORMAL SUBPROGRAMS MAY HAVE A PARAMETER
--     OF A GENERIC FORMAL TYPE, AND MAY RETURN A GENERIC FORMAL
--     TYPE.  CHECK MODES IN OUT AND OUT.

-- HISTORY:
--     BCB 08/04/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE CC1304B IS

BEGIN
     TEST ("CC1304B", "GENERIC FORMAL SUBPROGRAMS MAY HAVE A " &
                      "PARAMETER OF A GENERIC FORMAL TYPE, AND MAY  " &
                      "RETURN A GENERIC FORMAL TYPE.  CHECK MODES IN " &
                      "OUT AND OUT");

     DECLARE
          GENERIC
               TYPE T IS ( <> );
               WITH PROCEDURE S (P : OUT T);
               WITH PROCEDURE P (P : IN OUT T);
               WITH FUNCTION L RETURN T;
          PROCEDURE PR (PARM1, PARM2, PARM3 : IN OUT T);

          PROCEDURE PR (PARM1, PARM2, PARM3 : IN OUT T) IS
          BEGIN
               S (P => PARM1);
               P (P => PARM2);
               PARM3 := L;
          END PR;
     BEGIN
          DECLARE
               C : CHARACTER := 'A';
               C1 : CHARACTER := 'Y';
               C2 : CHARACTER := 'I';
               B : BOOLEAN := FALSE;
               B1 : BOOLEAN := TRUE;
               B2 : BOOLEAN := FALSE;
               I : INTEGER := 5;
               I1 : INTEGER := 10;
               I2 : INTEGER := 0;
               TYPE ENUM IS (E1, E2, E3);
               F : ENUM := E2;
               F1 : ENUM := E1;
               F2 : ENUM := E2;

               PROCEDURE FC (P : OUT CHARACTER) IS
               BEGIN
                    P := 'B';
               END FC;

               PROCEDURE FB (P : OUT BOOLEAN) IS
               BEGIN
                    P := NOT B;
               END FB;

               PROCEDURE FI (P : OUT INTEGER) IS
               BEGIN
                    P :=  I + 1;
               END FI;

               PROCEDURE FE (P : OUT ENUM) IS
               BEGIN
                    P := ENUM'SUCC (F);
               END FE;

               PROCEDURE  PC (P : IN OUT CHARACTER) IS
               BEGIN
                    P := 'Z';
               END PC;

               PROCEDURE PB (P : IN OUT BOOLEAN) IS
               BEGIN
                    P := NOT B1;
               END PB;

               PROCEDURE PI (P : IN OUT INTEGER) IS
               BEGIN
                   P := I1 + 1;
               END PI;

               PROCEDURE PE (P : IN OUT ENUM) IS
               BEGIN
                    P := ENUM'SUCC (F1);
               END PE;

               FUNCTION LC RETURN CHARACTER IS
               BEGIN
                    RETURN 'J';
               END LC;

               FUNCTION LB RETURN BOOLEAN IS
               BEGIN
                    RETURN TRUE;
               END LB;

               FUNCTION LI RETURN INTEGER IS
               BEGIN
                    RETURN IDENT_INT(5);
               END LI;

               FUNCTION LE RETURN ENUM IS
               BEGIN
                    RETURN ENUM'SUCC(F2);
               END LE;

               PACKAGE PKG2 IS
                    PROCEDURE P1 IS NEW PR (CHARACTER, FC, PC, LC);
                    PROCEDURE P2 IS NEW PR (BOOLEAN, FB, PB, LB);
                    PROCEDURE P3 IS NEW PR (INTEGER, FI, PI, LI);
                    PROCEDURE P4 IS NEW PR (ENUM, FE, PE, LE);
               END PKG2;

               PACKAGE BODY PKG2 IS
               BEGIN
                    P1 (C,C1,C2);
                    P2 (B,B1,B2);
                    P3 (I,I1,I2);
                    P4 (F,F1,F2);
               END  PKG2;
          BEGIN
               IF C /= 'B' OR B /= TRUE OR I /= 6 OR F /= E3 THEN
                    FAILED ("SUBPROGRAM PARAMETERS OF FORMAL TYPES - " &
                            "MODE OUT");
               END IF;

               IF C1 /= 'Z' OR B1 /= FALSE OR I1 /= 11 OR F1 /= E2 THEN
                    FAILED ("SUBPROGRAM PARAMETERS OF FORMAL TYPES - " &
                            "MODE IN OUT");
               END IF;

               IF C2 /= 'J' OR B2 /= TRUE OR I2 /= 5 OR F2 /= E3 THEN
                    FAILED ("GENERIC FORMAL SUBPROGRAMS RETURNING A " &
                            "GENERIC FORMAL TYPE");
               END IF;
          END;
     END;

     RESULT;
END CC1304B;
