-- CC1204A.ADA

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
-- CHECK THAT GENERIC FORMAL TYPES MAY HAVE A DISCRIMINANT PART,
-- WHICH MAY BE OF A GENERIC FORMAL TYPE.

-- DAT 8/14/81
-- SPS 5/12/82

WITH REPORT; USE REPORT;

PROCEDURE CC1204A IS
BEGIN
     TEST ("CC1204A", "DISCRIMINANT PARTS FOR GENERIC FORMAL TYPES");

     DECLARE
          GENERIC
               TYPE T IS ( <> );
               TYPE I IS RANGE <> ;
               TYPE R1 (C : BOOLEAN) IS PRIVATE;
               TYPE R2 (C : T) IS PRIVATE;
               TYPE R3 (C : I) IS LIMITED PRIVATE;
               P1 : IN R1;
               P2 : IN R2;
               V1 : IN OUT R1;
               V2 : IN OUT R2;
               V3 : IN OUT R3;
          PROCEDURE PROC;

          TYPE DD IS NEW INTEGER RANGE 1 .. 10;
          TYPE ARR IS ARRAY (DD RANGE <>) OF CHARACTER;
          TYPE RECD (C : DD := DD (IDENT_INT (1))) IS
               RECORD
                    C1 : ARR (1..C);
               END RECORD;

          X1 : RECD;
          X2 : RECD := (1, "Y");

          TYPE RECB (C : BOOLEAN) IS
               RECORD
                    V : INTEGER := 6;
               END RECORD;
          RB : RECB (IDENT_BOOL (TRUE));
          RB1 : RECB (IDENT_BOOL (TRUE));

          PROCEDURE PROC IS
          BEGIN
               IF P1.C /= TRUE
                  OR P2.C /= T'FIRST
                  OR V1.C /= TRUE
                  OR V2.C /= T'FIRST
                  OR V3.C /= I'FIRST
               THEN
                    FAILED ("WRONG GENERIC PARAMETER VALUE");
               END IF;

               V1 := P1;
               V2 := P2;

               IF V1 /= P1
                  OR V2 /= P2 
               THEN
                    FAILED ("BAD ASSIGNMENT TO GENERIC PARAMETERS");
               END IF;
          END PROC;

     BEGIN
          RB1.V := IDENT_INT (1);
          X1.C1 := "X";

          DECLARE
     
               PROCEDURE PR IS NEW PROC
                    (T => DD,
                     I => DD,
                     R1 => RECB,
                     R2 => RECD,
                     R3 => RECD,
                     P1 => RB1,
                     P2 => X1,
                     V1 => RB,
                     V2 => X2,
                     V3 => X2);
          BEGIN
               PR;
               IF RB /= (TRUE, 1) OR X2.C1 /= "X" THEN
                    FAILED ("PR NOT CALLED CORRECTLY");
               END IF;
          END;
     END;

     RESULT;
END CC1204A;
