-- CC3012A.ADA

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
-- CHECK THAT GENERIC INSTANCES MAY BE OVERLOADED.

-- CHECK THAT THEY MAY OVERLOAD PREVIOUSLY DECLARED SUBPROGRAMS AND
-- ENUMERATION LITERALS.

-- DAT 9/16/81
-- SPS 10/19/82
-- SPS 2/8/83
-- PWN 11/30/94 REMOVED PART OF TEST INVALID FOR ADA 9X.


WITH REPORT; USE REPORT;

PROCEDURE CC3012A IS
BEGIN
     TEST ("CC3012A", "CHECK THAT GENERIC INSTANCES MAY OVERLOAD " &
                      "OTHER IDENTIFIERS");

     DECLARE
          GENERIC 
               TYPE T IS ( <> );
               V : IN T;
          PROCEDURE GP (X : IN OUT T);

          GENERIC 
               TYPE T IS ( <> );
          FUNCTION LESS (X, Y : T) RETURN BOOLEAN;

          GENERIC
               TYPE T IS ( <> );
          FUNCTION PLUS (X, Y : T) RETURN T;

          GENERIC
               TYPE T IS PRIVATE;
               Z : T;
          FUNCTION F1  RETURN T;

          TYPE DC IS NEW CHARACTER RANGE IDENT_CHAR ('A') .. 'Z';
          TYPE DI IS NEW INTEGER;
          TYPE ENUM IS (E1, E2, E3, E4);

          VC : CHARACTER := 'A';
          VI : INTEGER := 5;
          VB : BOOLEAN := TRUE;
          VE : ENUM := E2;

          TYPE DENUM IS NEW ENUM RANGE E2 .. ENUM'LAST;

          VDE : DENUM := E4;
          VDC : DC := 'A';
          VDI : DI := 7;

          PROCEDURE GP (X : IN OUT T) IS
          BEGIN
               X := V;
          END GP;

          FUNCTION LESS (X, Y : T) RETURN BOOLEAN IS
          BEGIN
               RETURN FALSE;
          END LESS;

          FUNCTION PLUS (X, Y : T) RETURN T IS
          BEGIN
               RETURN T'FIRST;
          END PLUS;

          FUNCTION F1 RETURN T IS
          BEGIN
               RETURN Z;
          END F1;

          FUNCTION E5 RETURN INTEGER IS
          BEGIN
               RETURN 1;
          END E5;

          PACKAGE PKG IS
          
               PROCEDURE P IS NEW GP (CHARACTER, 'Q');
               PROCEDURE P IS NEW GP (INTEGER, -14);
               PROCEDURE P IS NEW GP (BOOLEAN, FALSE);
               PROCEDURE P IS NEW GP (ENUM, E4);
               PROCEDURE P IS NEW GP (DC, 'W');
               PROCEDURE P IS NEW GP (DI, -33);
               PROCEDURE P IS NEW GP (DENUM, E2);
     
               FUNCTION "<" IS NEW LESS (CHARACTER);
               FUNCTION "<" IS NEW LESS (INTEGER);
               FUNCTION "<" IS NEW LESS (BOOLEAN);
               FUNCTION "<" IS NEW LESS (ENUM);
               FUNCTION "<" IS NEW LESS (DC);
               FUNCTION "<" IS NEW LESS (DI);
               -- NOT FOR DENUM.
     
               FUNCTION "+" IS NEW PLUS (CHARACTER);
               FUNCTION "+" IS NEW PLUS (INTEGER);
               FUNCTION "+" IS NEW PLUS (BOOLEAN);
               FUNCTION "+" IS NEW PLUS (ENUM);
               FUNCTION "+" IS NEW PLUS (DC);
               -- NOT FOR DI.
               FUNCTION "+" IS NEW PLUS (DENUM);

               FUNCTION E2 IS NEW F1 (BOOLEAN, FALSE);
               FUNCTION E5 IS NEW F1 (DC, 'M');

          END PKG;

          PACKAGE BODY PKG IS
          BEGIN
               P (VC);
               P (VI);
               P (VB);
               P (VE);
               P (X => VDE);
               P (X => VDC);
               P (X => VDI);

               IF VC /= 'Q' THEN
                    FAILED ("OVERLOADED PROCEDURE - 1");
               END IF;

               IF VI /= -14 THEN
                    FAILED ("OVERLOADED PROCEDURE - 2");
               END IF;

               IF VB /= FALSE THEN
                    FAILED ("OVERLOADED PROCEDURE - 3");
               END IF;

               IF VE /= E4 THEN
                    FAILED ("OVERLOADED PROCEDURE - 4");
               END IF;

               IF VDE /= E2 THEN
                    FAILED ("OVERLOADED PROCEDURE - 5");
               END IF;

               IF VDC /= 'W' THEN
                    FAILED ("OVERLOADED PROCEDURE - 6");
               END IF;

               IF VDI /= -33 THEN
                    FAILED ("OVERLOADED PROCEDURE - 7");
               END IF;

               IF VC  < ASCII.DEL THEN
                    FAILED ("OVERLOADED LESS THAN - 1");
               END IF;

               IF VI < 1E3 THEN
                    FAILED ("OVERLOADED LESS THAN - 2");
               END IF;

               IF FALSE < TRUE THEN
                    FAILED ("OVERLOADED LESS THAN - 3");
               END IF;

               IF E1 < VE THEN
                    FAILED ("OVERLOADED LESS THAN - 4");
               END IF;

               IF VDC < 'Z' THEN
                    FAILED ("OVERLOADED LESS THAN - 5");
               END IF;

               IF VDI < 0 THEN
                    FAILED ("OVERLOADED LESS THAN - 6");
               END IF;


               IF -14 + 5 /= -9 THEN
                    FAILED ("OVERLOADED PLUS - 2");
               END IF;

               IF VI + 5 /= INTEGER'FIRST THEN
                    FAILED ("OVERLOADED PLUS - 3");
               END IF;

               IF VB + TRUE /= FALSE THEN
                    FAILED ("OVERLOADED PLUS - 4");
               END IF;

               IF VE + E2 /= E1 THEN
                    FAILED ("OVERLOADED PLUS - 5");
               END IF;

               IF DENUM'(E3) + E2 /= E2 THEN
                    FAILED ("OVERLOADED PLUS - 6");
               END IF;

               IF  VDC + 'B' /= 'A' THEN
                    FAILED ("OVERLOADED PLUS - 7");
               END IF;

               IF VDI + 14 /= -19 THEN       -- -33 + 14
                    FAILED ("OVERLOADED PLUS - 8");
               END IF;

               VI := E5;
               VDC := E5;
               VE := E2;
               VB := E2;
               IF VI /= 1 OR
                  VDC /= 'M' OR
                  VE /= ENUM'VAL(IDENT_INT(1)) OR
                  VB /= FALSE THEN
                    FAILED ("OVERLOADING OF ENUMERATION LITERALS " &
                            "AND PREDEFINED SUBPROGRAMS");
               END IF;
          END PKG;
     BEGIN
          DECLARE
               USE PKG;
          BEGIN
               IF NOT (VI + 5 < 11) THEN
               FAILED ("INCORRECT VISIBILITY OF GENERIC OVERLOADING");
               END IF;
          END;
     END;

     RESULT;
END CC3012A;
