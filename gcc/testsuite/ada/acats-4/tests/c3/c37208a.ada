-- C37208A.ADA     (RA #534/1)

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
-- FOR A TYPE WITH DEFAULT DISCRIMINANT VALUES, CHECK THAT A
-- DISCRIMINANT CONSTRAINT CAN BE OMITTED IN:
 
     -- AN OBJECT DECLARATION, AND HENCE ASSIGNMENTS TO THE OBJECT CAN 
     -- CHANGE ITS DISCRIMINANTS;
 
     -- A COMPONENT_DECLARATION IN A RECORD TYPE DEFINITION, AND HENCE
     -- ASSIGNMENTS TO THE COMPONENT CAN CHANGE THE VALUE OF ITS
     -- DISCRIMINANTS;
 
     -- A SUBTYPE INDICATION IN AN ARRAY TYPE DEFINITION, AND HENCE
     -- ASSIGNMENTS TO ONE OF THE COMPONENTS CAN CHANGE ITS 
     -- DISCRIMINANT VALUES;
 
     -- A FORMAL PARAMETER OF A SUBPROGRAM; EXCEPT FOR PARAMETERS OF
     -- MODE IN, THE 'CONSTRAINED ATTRIBUTE OF THE ACTUAL PARAMETER
     -- BECOMES THE 'CONSTRAINED ATTRIBUTE OF THE FORMAL PARAMETER;
     -- FOR IN OUT AND OUT PARAMETERS, IF THE 'CONSTRAINED ATTRIBUTE IS 
     -- FALSE, ASSIGNMENTS TO THE FORMAL PARAMETER CAN CHANGE THE 
     -- DISCRIMINANTS OF THE ACTUAL PARAMETER; IF THE 'CONSTRAINED 
     -- ATTRIBUTE IS TRUE, ASSIGNNMENTS THAT ATTEMPT TO CHANGE THE 
     -- DISCRIMINANTS OF THE ACTUAL PARAMETER RAISE CONSTRAINT_ERROR.
 
-- ASL 7/23/81
-- EDS 7/16/98    AVOID OPTIMIZATION
 
WITH REPORT;
PROCEDURE C37208A IS
 
     USE REPORT;
 
BEGIN
     TEST ("C37208A","DISCRIMINANT CONSTRAINT CAN BE OMITTED " &
           "FROM OBJECT DECLARATION, COMPONENT DECLARATION, SUBTYPE " &
           "INDICATION OR FORMAL SUBPROGRAM PARAMETER, IF THE TYPE " &
           "HAS DEFAULT DISCRIMINANTS");
 
     DECLARE
          TYPE REC1(DISC : INTEGER := 7) IS
               RECORD
                    NULL;
               END RECORD;
 
          TYPE REC2 IS
               RECORD
                    COMP : REC1;
               END RECORD;
 
          R : REC2;
          U1,U2,U3 : REC1 := (DISC => 3);
          C1,C2,C3 : REC1(3) := (DISC => 3);
          ARR : ARRAY(INTEGER RANGE 1..10) OF REC1;
          ARR2 : ARRAY (1..10) OF REC1(4);
 
          PROCEDURE PROC(P_IN : IN REC1;
                         P_OUT : OUT REC1;
                         P_IN_OUT : IN OUT REC1;
                         CONSTR : IN BOOLEAN) IS
          BEGIN
               IF P_OUT'CONSTRAINED /= CONSTR
                    OR P_IN_OUT'CONSTRAINED /= CONSTR THEN
                    FAILED ("CONSTRAINED ATTRIBUTES DO NOT MATCH " &
                              "FOR ACTUAL AND FORMAL PARAMETERS");
               END IF;

               IF P_IN'CONSTRAINED /= IDENT_BOOL(TRUE) THEN
                    FAILED ("'CONSTRAINED IS FALSE FOR IN " &
                            "PARAMETER");
               END IF;
 
               IF NOT CONSTR THEN     -- UNCONSTRAINED ACTUAL PARAM
                    P_OUT := (DISC => IDENT_INT(0));
                    P_IN_OUT := (DISC => IDENT_INT(0));
               ELSE
                    BEGIN
                         P_OUT := (DISC => IDENT_INT(0));
                         FAILED ("DISCRIMINANT OF CONSTRAINED ACTUAL " &
                                 "PARAMETER ILLEGALLY CHANGED - 1");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION - 1");
                    END;

                    BEGIN
                         P_IN_OUT := (DISC => IDENT_INT(0));
                         FAILED ("DISCRIMINANT OF CONSTRAINED ACTUAL " &
                                 "PARAMETER ILLEGALLY CHANGED - 2");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR => NULL;
                         WHEN OTHERS =>
                              FAILED ("WRONG EXCEPTION - 2");
                    END;
               END IF;
          END PROC;
     BEGIN
          IF U1.DISC /= IDENT_INT(3) THEN
               FAILED ("INITIAL DISCRIMINANT VALUE WRONG - U1");
          END IF;

          U1 := (DISC => IDENT_INT(5));
          IF U1.DISC /= 5 THEN
               FAILED ("ASSIGNMENT FAILED FOR OBJECT");
          END IF;
 
          IF R.COMP.DISC /= IDENT_INT(7) THEN
               FAILED ("DEFAULT DISCRIMINANT VALUE WRONG - R");
          END IF;

          R.COMP := (DISC => IDENT_INT(5));
          IF R.COMP.DISC /= 5 THEN
               FAILED ("ASSIGNMENT FAILED FOR RECORD COMPONENT");
          END IF;
 
          FOR I IN 1..10 LOOP
               IF ARR(I).DISC /= IDENT_INT(7) THEN
                    FAILED ("DEFAULT DISCRIMINANT VALUE WRONG - ARR");
               END IF;
          END LOOP;

          ARR(3) := (DISC => IDENT_INT(5));
          IF ARR(3).DISC /= 5 THEN
               FAILED ("ASSIGNMENT FAILED FOR ARRAY COMPONENT");
          END IF;
 
          IF ARR /= (1..2|4..10 => (DISC => 7), 3 => (DISC => 5)) THEN
               FAILED ("MODIFIED WRONG COMPONENTS");
          END IF;

          PROC(C1,C2,C3,IDENT_BOOL(TRUE));
          PROC(U1,U2,U3,IDENT_BOOL(FALSE));
          IF U2.DISC /= 0 OR U3.DISC /= 0 THEN
               FAILED ("ASSIGNMENT TO UNCONSTRAINED ACTUAL PARAMETER " &
                       "FAILED TO CHANGE DISCRIMINANT");
          END IF;

          PROC(ARR(1), ARR(3), ARR(4), FALSE);
          IF ARR(3).DISC /= 0 OR ARR(4).DISC /= 0 THEN
               FAILED ("ARRAY COMPONENT ASSIGNMENTS DIDN'T CHANGE " &
                       "DISCRIMINANT OF COMPONENT");
          END IF;

          PROC (ARR2(2), ARR2(5), ARR2(10), TRUE);
     END;
 
     RESULT;
END C37208A;
