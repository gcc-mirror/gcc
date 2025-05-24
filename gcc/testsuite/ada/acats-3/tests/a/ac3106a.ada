-- AC3106A.ADA

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
--     CHECK THAT AN ACTUAL GENERIC IN OUT PARAMETER CAN BE:
--          A) ANY SUBCOMPONENT THAT DOES NOT DEPEND ON A DISCRIMINANT,
--             EVEN IF THE ENCLOSING VARIABLE IS UNCONSTRAINED;
--          B) ANY SUBCOMPONENT OF AN UNCONSTAINED VARIABLE OF A
--             RECORD TYPE IF THE DISCRIMINANTS OF THE
--             VARIABLE DO NOT HAVE DEFAULTS AND THE VARIABLE IS NOT
--             A GENERIC FORMAL IN OUT PARAMETER;
--          C) ANY COMPONENT OF AN OBJECT DESIGNATED BY AN ACCESS
--             VALUE.

-- HISTORY:
--     RJW 11/07/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE AC3106A IS

     SUBTYPE INT IS INTEGER RANGE 0 .. 10;

     TYPE REC (D : INT := 0) IS RECORD
          A : INTEGER := 5;
          CASE D IS
               WHEN OTHERS =>
                    V : INTEGER := 5;
          END CASE;
     END RECORD;

     TYPE AR_REC IS ARRAY (1 .. 10) OF REC;

     TYPE R_REC IS RECORD
          E : REC;
     END RECORD;

     TYPE A_STRING IS ACCESS STRING;
     TYPE A_REC IS ACCESS REC;
     TYPE A_AR_REC IS ACCESS AR_REC;
     TYPE A_R_REC IS ACCESS R_REC;

     TYPE DIS (L : INT := 1) IS RECORD
          S : STRING (1 .. L) := "A";
          R : REC (L);
          AS : A_STRING (1 .. L) := NEW STRING (1 .. L);
          AR : A_REC (L) := NEW REC (1);
          RC : REC (3);
          ARU : A_REC := NEW REC;
          V_AR : AR_REC;
          V_R : R_REC;
          AC_AR : A_AR_REC := NEW AR_REC;
          AC_R : A_R_REC := NEW R_REC;
     END RECORD;

     TYPE A_DIS IS ACCESS DIS;
     AD : A_DIS := NEW DIS;

     TYPE DIS2 (L : INT) IS RECORD
          S : STRING (1 .. L);
          R : REC (L);
          AS : A_STRING (1 .. L);
          AR : A_REC (L);
     END RECORD;

     X : DIS;

     SUBTYPE REC3 IS REC (3);

     GENERIC
          GREC3 : IN OUT REC3;
     PACKAGE PREC3 IS END PREC3;

     SUBTYPE REC0 IS REC (0);

     GENERIC
          GREC0 : IN OUT REC0;
     PACKAGE PREC0 IS END PREC0;

     GENERIC
          GINT : IN OUT INTEGER;
     PACKAGE PINT IS END PINT;

     GENERIC
          GA_REC : IN OUT A_REC;
     PACKAGE PA_REC IS END PA_REC;

     GENERIC
          GAR_REC : IN OUT AR_REC;
     PACKAGE PAR_REC IS END PAR_REC;

     GENERIC
          GR_REC : IN OUT R_REC;
     PACKAGE PR_REC IS END PR_REC;

     GENERIC
          GA_AR_REC : IN OUT A_AR_REC;
     PACKAGE PA_AR_REC IS END PA_AR_REC;

     GENERIC
          GA_R_REC : IN OUT A_R_REC;
     PACKAGE PA_R_REC IS END PA_R_REC;

     TYPE BUFFER (SIZE : INT) IS RECORD
          POS : NATURAL := 0;
          VAL : STRING (1 .. SIZE);
     END RECORD;

     SUBTYPE BUFF_5 IS BUFFER (5);

     GENERIC
          Y : IN OUT CHARACTER;
     PACKAGE P_CHAR IS END P_CHAR;

     SUBTYPE STRING5 IS STRING (1 .. 5);
     GENERIC
          GSTRING : STRING5;
     PACKAGE P_STRING IS END P_STRING;

     GENERIC
          GA_STRING : A_STRING;
     PACKAGE P_A_STRING IS END P_A_STRING;

     GENERIC
          X : IN OUT BUFF_5;
     PACKAGE P_BUFF IS
          RX : BUFF_5 RENAMES X;
     END P_BUFF;

     Z : BUFFER (1) := (SIZE => 1, POS =>82, VAL =>"R");
BEGIN
     TEST ("AC3106A", "CHECK THE PERMITTED FORMS OF AN ACTUAL " &
                      "GENERIC IN OUT PARAMETER");

     DECLARE -- A)
          PACKAGE NPINT3 IS NEW PINT (X.RC.A);
          PACKAGE NPINT4 IS NEW PINT (X.RC.V);
          PACKAGE NPREC3 IS NEW PREC3 (X.RC);
          PACKAGE NPA_REC IS NEW PA_REC (X.ARU);
          PACKAGE NPINT5 IS NEW PINT (X.ARU.A);
          PACKAGE NPINT6 IS NEW PINT (X.ARU.V);
          PACKAGE NPAR_REC IS NEW PAR_REC (X.V_AR);
          PACKAGE NPREC01 IS NEW PREC0 (X.V_AR (1));
          PACKAGE NPR_REC IS NEW PR_REC (X.V_R);
          PACKAGE NPREC02 IS NEW PREC0 (X.V_R.E);
          PACKAGE NPINT7 IS NEW PINT (X.V_R.E.A);

          PACKAGE NP_BUFF IS NEW P_BUFF (Z);
          USE NP_BUFF;

          PACKAGE NP_CHAR3 IS NEW P_CHAR (RX.VAL (1));

          PROCEDURE PROC (X : IN OUT BUFFER) IS
               PACKAGE NP_CHAR4 IS NEW P_CHAR (X.VAL (1));
          BEGIN
               NULL;
          END;
     BEGIN
          NULL;
     END; -- A)

     DECLARE -- B)
          PROCEDURE PROC (Y : IN OUT DIS2) IS
               PACKAGE NP_STRING IS NEW P_STRING (Y.S);
               PACKAGE NP_CHAR IS NEW P_CHAR (Y.S (1));
               PACKAGE NP_A_STRING IS NEW P_A_STRING (Y.AS);
               PACKAGE NP_CHAR2 IS NEW P_CHAR (Y.AS (1));
               PACKAGE NPINT3 IS NEW PINT (Y.R.A);
               PACKAGE NPINT4 IS NEW PINT (Y.R.V);
               PACKAGE NPREC3 IS NEW PREC3 (Y.R);
               PACKAGE NPA_REC IS NEW PA_REC (Y.AR);
               PACKAGE NPINT5 IS NEW PINT (Y.AR.A);
               PACKAGE NPINT6 IS NEW PINT (Y.AR.V);
          BEGIN
               NULL;
          END;
     BEGIN
          NULL;
     END; -- B)

     DECLARE -- C)
          PACKAGE NP_CHAR IS NEW P_CHAR (AD.S (1));
          PACKAGE NP_A_STRING IS NEW P_A_STRING (AD.AS);
          PACKAGE NP_CHAR2 IS NEW P_CHAR (AD.AS (1));
          PACKAGE NPINT3 IS NEW PINT (AD.R.A);
          PACKAGE NPINT4 IS NEW PINT (AD.R.V);
          PACKAGE NPREC3 IS NEW PREC3 (AD.R);
          PACKAGE NPA_REC IS NEW PA_REC (AD.AR);
          PACKAGE NPINT5 IS NEW PINT (AD.AR.A);
          PACKAGE NPINT6 IS NEW PINT (AD.AR.V);
     BEGIN
          NULL;
     END; -- C)

     RESULT;
END AC3106A;
