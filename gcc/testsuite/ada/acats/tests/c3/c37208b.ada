-- C37208B.ADA

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
-- DISCRIMINANT CONSTRAINT CAN BE OMITTED IN A GENERIC FORMAL
-- PARAMETER, AND HENCE, FOR BOTH IN AND IN OUT PARAMETERS, THE
-- 'CONSTRAINED ATTRIBUTE OF THE ACTUAL PARAMETER BECOMES THE
-- 'CONSTRAINED ATTRIBUTE OF THE FORMAL PARAMETER, AND, FOR IN
-- OUT PARAMETERS, IF THE 'CONSTRAINED ATTRIBUTE IS FALSE,
-- ASSIGNMENTS TO THE FORMAL PARAMETERS CAN CHANGE THE
-- DISCRIMINANTS OF THE ACTUAL PARAMETER; IF THE 'CONSTRAINED
-- ATTRIBUTE IS  TRUE, ASSIGNMENTS THAT ATTEMPT TO CHANGE THE
-- DISCRIMINANTS OF THE ACTUAL PARAMETER RAISE CONSTRAINT_ERROR.
 
-- ASL 7/29/81
-- VKG 1/20/83
-- EDS 7/16/98    AVOID OPTIMIZATION

WITH REPORT;
PROCEDURE C37208B IS
 
     USE REPORT;
 
BEGIN
     TEST ("C37208B","FOR TYPES WITH DEFAULT DISCRIMINANT " &
            "VALUES, DISCRIMINANT CONSTRAINTS CAN BE OMITTED " &
            "IN GENERIC FORMAL PARAMETERS, AND THE " &
            "'CONSTRAINED ATTRIBUTE HAS CORRECT VALUES " &
            "DEPENDING ON THE ACTUAL PARAMETERS");
 
     DECLARE
          TYPE REC(DISC : INTEGER := 7) IS
               RECORD
                    NULL;
               END RECORD;
 
          KC : CONSTANT REC(3) := (DISC => 3);
          KU : CONSTANT REC := (DISC => 3);
          OBJC1,OBJC2 : REC(3) := (DISC => 3);
          OBJU1,OBJU2 : REC := (DISC => 3);
 
          GENERIC
               P_IN1 : REC;
               P_IN2 : REC;
               P_IN_OUT : IN OUT REC;
               STATUS : BOOLEAN;
          PROCEDURE PROC;

          PROCEDURE PROC IS
          BEGIN

               IF P_IN1'CONSTRAINED /= TRUE  OR
                  P_IN2'CONSTRAINED /= TRUE  OR
                  P_IN_OUT'CONSTRAINED /= STATUS
               THEN

                    FAILED ("'CONSTRAINED ATTRIBUTES DO NOT MATCH " &
                            "FOR ACTUAL AND FORMAL PARAMETERS");
               END IF;
               IF NOT STATUS THEN
                    BEGIN
                         P_IN_OUT := (DISC => IDENT_INT(7));
                    EXCEPTION 
                         WHEN OTHERS =>
                              FAILED ("EXCEPTION RAISED " &
                                      "WHEN TRYING TO " &
                                      "CHANGE UNCONSTRAINED " &
                                      "DISCRIMINANT VALUE");
                    END;
               ELSE
                    BEGIN
                         P_IN_OUT := (DISC => IDENT_INT(7));
                         FAILED ("DISCRIMINANT OF CONSTRAINED " &
                                 "ACTUAL PARAMETER ILLEGALLY " &
                                 "CHANGED BY ASSIGNMENT");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR => NULL;     
                         WHEN OTHERS => FAILED ("WRONG EXCEPTION");
                    END;
               END IF;
          END PROC;

     BEGIN

          DECLARE
               PROCEDURE PROC_C IS NEW PROC(KC,OBJC1,OBJC2,IDENT_BOOL(TRUE));
               PROCEDURE PROC_U IS NEW PROC(KU,OBJU1,OBJU2,IDENT_BOOL(FALSE));
          BEGIN
               PROC_C;
               PROC_U;
               IF OBJU2.DISC /= 7 THEN
               FAILED ("ASSIGNMENT TO UNCONSTRAINED ACTUAL " &
                       "PARAMETER FAILED TO CHANGE DISCRIMINANT ");
               END IF;
          END;

     END;
     RESULT;
END C37208B;
