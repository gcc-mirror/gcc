-- CC3601C.ADA

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
-- CHECK THAT "/=" MAY BE PASSED AS A GENERIC ACTUAL FUNCTION
-- PARAMETER.

-- DAT 10/6/81
-- SPS 10/27/82
-- JRK 2/9/83

WITH REPORT; USE REPORT;

PROCEDURE CC3601C IS
BEGIN
     TEST ("CC3601C", "/= AS GENERIC ACTUAL PARAMETER");

     DECLARE
          PACKAGE PK IS
               TYPE LP IS LIMITED PRIVATE;
               FUNCTION "=" (X, Y : LP) RETURN BOOLEAN;-- RETURNS FALSE.
               TYPE INT IS NEW INTEGER;
          PRIVATE
               TASK TYPE LP;
          END PK;
          USE PK;

          V1, V2 : LP;

          TYPE REC IS RECORD
               C : LP;
          END RECORD;

          R1, R2 : REC;

          TYPE INT IS NEW INTEGER;

          B1 : BOOLEAN := TRUE;
          B2 : BOOLEAN := TRUE;
          INTEGER_3 : INTEGER := 3;
          INTEGER_4 : INTEGER := 4;
          INT_3     : INT := 3;
          INT_4     : INT := 4;
          INT_5     : INT := 5;
          PK_INT_M1 : PK.INT := -1;
          PK_INT_M2 : PK.INT := -2;
          PK_INT_1  : PK.INT := 1;
          PK_INT_2  : PK.INT := 2;
          PK_INT_3  : PK.INT := 3;

          FUNCTION "=" (Q, R : LP) RETURN BOOLEAN;-- RETURNS TRUE.

          GENERIC
               TYPE T IS LIMITED PRIVATE;
               V1, V2 : IN OUT T;
               WITH FUNCTION NE (ZA : IN T; ZB : T) RETURN BOOLEAN;
               VALUE : IN BOOLEAN; -- SHOULD BE VALUE OF NE(V1,V2).
               STR : STRING;
          PACKAGE GP IS END GP;

          FUNCTION "=" (Q, R : IN REC) RETURN BOOLEAN;

          FUNCTION NE (Q : INT; R : IN INT) RETURN BOOLEAN
               RENAMES "/=";

          FUNCTION NE (Q : PK.INT; R : IN PK.INT) RETURN BOOLEAN
               RENAMES "/=";

          PACKAGE BODY GP IS
          BEGIN
               IF IDENT_BOOL(VALUE) /= NE (V1, V2) THEN
                    FAILED ("WRONG /= ACTUAL GENERIC PARAMETER "
                    & STR);
               END IF;
          END GP;

          FUNCTION "=" (Q, R : IN REC) RETURN BOOLEAN IS
          BEGIN
               RETURN FALSE;
          END "=";

          FUNCTION "=" (Q, R : LP) RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END "=";

          PACKAGE BODY PK IS
               FUNCTION "=" (X, Y : LP) RETURN BOOLEAN IS
               BEGIN
                    RETURN R1 = R1;     -- FALSE.
               END "=";
               TASK BODY LP IS BEGIN NULL; END;
          END PK;

          PACKAGE P1 IS NEW GP (LP, V1, V2, "/=", FALSE, "1");

          FUNCTION "NOT" (X : BOOLEAN) RETURN BOOLEAN IS
          BEGIN RETURN X; END "NOT"; -- ENSURES USE OF PREDEFINED "NOT"

          PACKAGE P2 IS NEW GP (LP,      V1, V2, "/=", FALSE, "2");
          PACKAGE P3 IS NEW GP (LP, V1, V2, PK."/=", TRUE, "3");
          PACKAGE P4 IS NEW GP (PK.LP, V1, V2, "/=", FALSE, "4");
          PACKAGE P5 IS NEW GP (PK.LP, V1, V2, PK."/=", TRUE, "5");
          PACKAGE P6 IS NEW GP (REC, R1, R2, "/=", TRUE, "6");
          PACKAGE P7 IS NEW GP (INTEGER, INTEGER_3, INTEGER_4, "/=",
                                TRUE, "7");
          PACKAGE P8 IS NEW GP (BOOLEAN, B1, B2, "/=", FALSE,"8");
          PACKAGE P9 IS NEW GP (INT, INT_3, INT_5, "/=", TRUE, "9");
          PACKAGE P10 IS NEW GP (INT, INT_3, INT_3, "/=", FALSE, "10");
          PACKAGE P11 IS NEW GP (INT, INT_3, INT_4, NE, TRUE, "11");
          PACKAGE P12 IS NEW GP (INT, INT_3, INT_3, NE, FALSE, "12");
          PACKAGE P13 IS NEW GP (PK.INT, PK_INT_3, PK_INT_3, NE,
                                 FALSE, "13");
          PACKAGE P14 IS NEW GP (PK.INT, PK_INT_M1, PK_INT_M2, NE, 
                                 TRUE,  "14");
          PACKAGE P15 IS NEW GP (PK.INT, PK_INT_1, PK_INT_1, "/=",
                                 FALSE, "15");
          PACKAGE P16 IS NEW GP (PK.INT, PK_INT_1, PK_INT_2, "/=",
                                 TRUE,  "16");
          PACKAGE P17 IS NEW GP (PK.INT, PK_INT_1, PK_INT_1, PK."/=",
                                 FALSE, "17");
          PACKAGE P18 IS NEW GP (PK.INT, PK_INT_1, PK_INT_2, PK."/=",
                                 TRUE,  "18");
     BEGIN
          NULL;
     END;

     RESULT;
END CC3601C;
