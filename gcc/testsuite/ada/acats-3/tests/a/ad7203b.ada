-- AD7203B.ADA

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
--     CHECK THAT THE PREFIX OF THE 'SIZE' ATTRIBUTE CAN BE AN OBJECT,
--     A TYPE, OR A SUBTYPE.

-- HISTORY:
--     BCB  09/27/88  CREATED ORIGINAL TEST BY MODIFYING AND RENAMING
--                    CD7203B.ADA.

WITH SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE AD7203B IS

     TYPE I_REC IS
          RECORD
               I1, I2 : INTEGER;
          END RECORD;

     I   : INTEGER;
     I_A : ARRAY (1 ..5) OF INTEGER;
     I_R : I_REC;

     I_SIZE : INTEGER := I'SIZE;
     I_A_SIZE : INTEGER := I_A'SIZE;
     I_R_SIZE : INTEGER := I_R'SIZE;
     I_A_1_SIZE : INTEGER := I_A(1)'SIZE;
     I_R_I1_SIZE : INTEGER := I_R.I1'SIZE;

     TYPE FIXED IS DELTA 0.01 RANGE -1.0 .. 1.0;
     TYPE FXD_REC IS
          RECORD
               FXD1, FXD2 : FIXED;
          END RECORD;

     FXD   : FIXED;
     FXD_A : ARRAY (1 .. 5) OF FIXED;
     FXD_R : FXD_REC;

     FXD_SIZE : INTEGER := FXD'SIZE;
     FXD_A_SIZE : INTEGER := FXD_A'SIZE;
     FXD_R_SIZE : INTEGER := FXD_R'SIZE;
     FXD_A_1_SIZE : INTEGER := FXD_A(1)'SIZE;
     FXD_R_FXD1_SIZE : INTEGER := FXD_R.FXD1'SIZE;

     TYPE FLT_REC IS
          RECORD
               FLT1, FLT2 : FLOAT;
          END RECORD;

     FLT   : FLOAT;
     FLT_A : ARRAY (1 .. 5) OF FLOAT;
     FLT_R : FLT_REC;

     FLT_SIZE : INTEGER := FLT'SIZE;
     FLT_A_SIZE : INTEGER := FLT_A'SIZE;
     FLT_R_SIZE : INTEGER := FLT_R'SIZE;
     FLT_A_1_SIZE : INTEGER := FLT_A(1)'SIZE;
     FLT_R_FLT1_SIZE : INTEGER := FLT_R.FLT1'SIZE;

     SUBTYPE TINY_INT IS INTEGER RANGE 0 .. 255;
     TYPE TI_REC IS
          RECORD
               TI1, TI2 : TINY_INT;
          END RECORD;

     TI   : TINY_INT;
     TI_A : ARRAY (1 .. 5) OF TINY_INT;
     TI_R : TI_REC;

     TINY_INT_SIZE : INTEGER := TINY_INT'SIZE;
     TI_SIZE : INTEGER := TI'SIZE;
     TI_A_SIZE : INTEGER := TI_A'SIZE;
     TI_R_SIZE : INTEGER := TI_R'SIZE;
     TI_A_1_SIZE : INTEGER := TI_A(1)'SIZE;
     TI_R_TI1_SIZE : INTEGER := TI_R.TI1'SIZE;

     TYPE STR IS ARRAY (TINY_INT RANGE <>) OF CHARACTER;
     TYPE STR_2 IS ARRAY (1 .. 127) OF CHARACTER;
     TYPE STR_REC IS
          RECORD
               S1, S2 : STR (TINY_INT'FIRST .. TINY_INT'LAST);
          END RECORD;

     S   : STR (TINY_INT'FIRST .. TINY_INT'LAST);
     S_A : ARRAY (1 .. 5) OF STR (TINY_INT'FIRST .. TINY_INT'LAST);
     S_R : STR_REC;

     STR_2_SIZE : INTEGER := STR_2'SIZE;
     S_SIZE : INTEGER := S'SIZE;
     S_A_SIZE : INTEGER := S_A'SIZE;
     S_R_SIZE : INTEGER := S_R'SIZE;
     S_A_1_SIZE : INTEGER := S_A(1)'SIZE;
     S_R_S1_SIZE : INTEGER := S_R.S1'SIZE;

     TYPE C_REC IS
          RECORD
               C1, C2 : CHARACTER;
          END RECORD;

     C   : CHARACTER;
     C_A : ARRAY (1 .. 5) OF CHARACTER;
     C_R : C_REC;

     C_SIZE : INTEGER := C'SIZE;
     C_A_SIZE : INTEGER := C_A'SIZE;
     C_R_SIZE : INTEGER := C_R'SIZE;
     C_A_1_SIZE : INTEGER := C_A(1)'SIZE;
     C_R_C1_SIZE : INTEGER := C_R.C1'SIZE;

     TYPE B_REC IS
          RECORD
               B1, B2 : BOOLEAN;
          END RECORD;

     B   : BOOLEAN;
     B_A : ARRAY (1 .. 5) OF BOOLEAN;
     B_R : B_REC;

     B_SIZE : INTEGER := B'SIZE;
     B_A_SIZE : INTEGER := B_A'SIZE;
     B_R_SIZE : INTEGER := B_R'SIZE;
     B_A_1_SIZE : INTEGER := B_A(1)'SIZE;
     B_R_B1_SIZE : INTEGER := B_R.B1'SIZE;

     TYPE DISCR IS RANGE 1 .. 2;
     TYPE DISCR_REC (D : DISCR := 1) IS
          RECORD
               CASE D IS
                    WHEN 1 =>
                         C1_I : INTEGER;
                    WHEN 2 =>
                         C2_I1 : INTEGER;
                         C2_I2 : INTEGER;
               END CASE;
          END RECORD;

     DR_UC : DISCR_REC;
     DR_C  : DISCR_REC (2);
     DR_A  : ARRAY (1 .. 5) OF DISCR_REC;

     DR_UC_SIZE : INTEGER := DR_UC'SIZE;
     DR_C_SIZE : INTEGER := DR_C'SIZE;
     DR_A_SIZE : INTEGER := DR_A'SIZE;
     DR_UC_C1_I_SIZE : INTEGER := DR_UC.C1_I'SIZE;
     DR_A_1_SIZE : INTEGER := DR_A(1)'SIZE;

     TYPE ENUM IS (E1, E2, E3, E4);
     TYPE ENUM_REC IS
          RECORD
               E1, E2 : ENUM;
          END RECORD;

     E   : ENUM;
     E_A : ARRAY (1 .. 5) OF ENUM;
     E_R : ENUM_REC;

     E_SIZE : INTEGER := E'SIZE;
     E_A_SIZE : INTEGER := E_A'SIZE;
     E_R_SIZE : INTEGER := E_R'SIZE;
     E_A_1_SIZE : INTEGER := E_A(1)'SIZE;
     E_R_E1_SIZE : INTEGER := E_R.E1'SIZE;

     TASK TYPE TSK IS END TSK;
     TYPE TSK_REC IS
          RECORD
               TSK1, TSK2 : TSK;
          END RECORD;

     T   : TSK;
     T_A : ARRAY (1 .. 5) OF TSK;
     T_R : TSK_REC;

     T_SIZE : INTEGER := T'SIZE;
     T_A_SIZE : INTEGER := T_A'SIZE;
     T_R_SIZE : INTEGER := T_R'SIZE;
     T_A_1_SIZE : INTEGER := T_A(1)'SIZE;
     T_R_TSK1_SIZE : INTEGER := T_R.TSK1'SIZE;

     TYPE ACC IS ACCESS INTEGER;
     TYPE ACC_REC IS
          RECORD
               A1, A2 : ACC;
          END RECORD;

     A   : ACC;
     A_A : ARRAY (1 .. 5) OF ACC;
     A_R : ACC_REC;

     A_SIZE : INTEGER := A'SIZE;
     A_A_SIZE : INTEGER := A_A'SIZE;
     A_R_SIZE : INTEGER := A_R'SIZE;
     A_A_1_SIZE : INTEGER := A_A(1)'SIZE;
     A_R_A1_SIZE : INTEGER := A_R.A1'SIZE;

     PACKAGE PK IS
          TYPE PRV IS PRIVATE;
          TYPE PRV_REC IS
               RECORD
                    P1, P2 : PRV;
               END  RECORD;

          TYPE LPRV IS LIMITED PRIVATE;
          TYPE LPRV_REC IS
               RECORD
                    LP1, LP2 : LPRV;
               END RECORD;
     PRIVATE
          TYPE PRV IS NEW INTEGER;

          TYPE LPRV IS NEW INTEGER;
     END PK;
     USE PK;

     P   : PRV;
     P_A : ARRAY (1 .. 5) OF PRV;
     P_R : PRV_REC;

     P_SIZE : INTEGER := P'SIZE;
     P_A_SIZE : INTEGER := P_A'SIZE;
     P_R_SIZE : INTEGER := P_R'SIZE;
     P_A_1_SIZE : INTEGER := P_A(1)'SIZE;
     P_R_P1_SIZE : INTEGER := P_R.P1'SIZE;

     LP   : LPRV;
     LP_A : ARRAY (1 .. 5) OF LPRV;
     LP_R : LPRV_REC;

     LP_SIZE : INTEGER := LP'SIZE;
     LP_A_SIZE : INTEGER := LP_A'SIZE;
     LP_R_SIZE : INTEGER := LP_R'SIZE;
     LP_A_1_SIZE : INTEGER := LP_A(1)'SIZE;
     LP_R_LP1_SIZE : INTEGER := LP_R.LP1'SIZE;

     TASK BODY TSK IS
     BEGIN
          NULL;
     END TSK;

BEGIN
     TEST ("AD7203B", "CHECK THAT THE PREFIX OF THE 'SIZE' ATTRIBUTE " &
                      "CAN BE AN OBJECT, A TYPE, OR A SUBTYPE");

     RESULT;
END AD7203B;
