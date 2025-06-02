-- C38002B.ADA

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
--     CHECK THAT AN UNCONSTRAINED ARRAY TYPE OR A RECORD WITHOUT
--     DEFAULT DISCRIMINANTS CAN BE USED IN AN ACCESS_TYPE_DEFINITION
--     WITHOUT AN INDEX OR DISCRIMINANT CONSTRAINT.
--
--     CHECK THAT (NON-STATIC) INDEX OR DISCRIMINANT CONSTRAINTS CAN
--     SUBSEQUENTLY BE IMPOSED WHEN THE TYPE IS USED IN AN OBJECT
--     DECLARATION, ARRAY COMPONENT DECLARATION, RECORD COMPONENT
--     DECLARATION, ACCESS TYPE DECLARATION, PARAMETER DECLARATION,
--     ALLOCATOR, DERIVED TYPE DEFINITION, PRIVATE TYPE, OR AS THE
--     RETURN TYPE IN A FUNCTION DECLARATION.
--
--     CHECK FOR GENERIC FORMAL ACCESS TYPES.

-- HISTORY:
--     AH  09/02/86 CREATED ORIGINAL TEST.
--     DHH 08/22/88 REVISED HEADER, ADDED 'PRIVATE TYPE' TO COMMENTS
--                  AND CORRECTED INDENTATION.

WITH REPORT; USE REPORT;
PROCEDURE C38002B IS

     C3 : CONSTANT INTEGER := IDENT_INT(3);

     TYPE UNCON_ARR IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     TYPE REC (DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE P_ARR_NAME IS ACCESS UNCON_ARR;
     TYPE P_REC_NAME IS ACCESS REC;

     GENERIC
          TYPE ACC_REC IS ACCESS REC;
          TYPE ACC_ARR IS ACCESS UNCON_ARR;
     PACKAGE P IS
          OBJ : ACC_REC(C3);

          TYPE ARR2 IS ARRAY (1..10) OF ACC_REC(C3);

          TYPE REC1 IS
               RECORD
                    COMP1 : ACC_REC(C3);
               END RECORD;

          TYPE REC2 IS
               RECORD
                    COMP2 : ACC_ARR(1..C3);
               END RECORD;

          SUBTYPE ACC_REC_3 IS ACC_REC(C3);
          R : ACC_REC;

          FUNCTION F (PARM : ACC_REC_3) RETURN ACC_REC_3;

          TYPE ACC1 IS PRIVATE;
          TYPE ACC2 IS PRIVATE;
          TYPE DER1 IS PRIVATE;
          TYPE DER2 IS PRIVATE;

     PRIVATE

          TYPE ACC1 IS ACCESS ACC_REC(C3);
          TYPE ACC2 IS ACCESS ACC_ARR(1..C3);
          TYPE DER1 IS NEW ACC_REC(C3);
          TYPE DER2 IS NEW ACC_ARR(1..C3);
     END P;

     PACKAGE BODY P IS
          FUNCTION F (PARM : ACC_REC_3) RETURN ACC_REC_3 IS
          BEGIN
               RETURN PARM;
          END;
     END P;

     PACKAGE NP IS NEW P (ACC_REC => P_REC_NAME, ACC_ARR => P_ARR_NAME);

     USE NP;
BEGIN
     TEST ("C38002B", "NON-STATIC CONSTRAINTS CAN BE IMPOSED " &
           "ON ACCESS TYPES ACCESSING PREVIOUSLY UNCONSTRAINED " &
           "ARRAY OR RECORD TYPES");

     R := NEW REC(DISC => 3);
     R := F(R);
     R := NEW REC(DISC => 4);
     R := F(R);
     FAILED ("INCOMPATIBLE CONSTRAINT ON ACCESS VALUE ACCEPTED " &
             "BY GENERIC FUNCTION");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF R = NULL OR ELSE R.DISC /= 4 THEN
                    FAILED (" ERROR IN EVALUATION/ASSIGNMENT OF " &
                            "GENERIC ACCESS VALUE");
               END IF;

     RESULT;
END C38002B;
