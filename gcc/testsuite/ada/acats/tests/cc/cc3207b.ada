-- CC3207B.ADA

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
--     CHECK THAT INSTANTIATION IS LEGAL IF A FORMAL
--     PARAMETER HAVING A LIMITED PRIVATE TYPE WITHOUT
--     A DISCRIMINANT IS USED TO DECLARE AN ACCESS
--     TYPE IN A BLOCK THAT CONTAINS A SELECTIVE WAIT
--     WITH A TERMINATE ALTERNATIVE, AND ACTUAL
--     PARAMETER'S BASE IS A TASK TYPE OR TYPE WITH A
--     SUBCOMPONENT OF A TASK TYPE.

-- HISTORY:
--     LDC  06/24/88  CREATED ORIGINAL TEST.

WITH REPORT;
USE REPORT;

PROCEDURE CC3207B IS
BEGIN
     TEST("CC3207B","CHECK THAT INSTANTIATION IS LEGAL IF A "     &
                    "FORMAL PARAMETER HAVING A LIMITED PRIVATE "  &
                    "TYPE WITHOUT A DISCRIMINANT IS USED TO "     &
                    "DECLARE AN ACCESS TYPE IN A BLOCK THAT "     &
                    "CONTAINS A SELECTIVE WAIT WITH A TERMINATE " &
                    "ALTERNATIVE, AND ACTUAL PARAMETER'S BASE "   &
                    "A TASK TYPE OR TYPE WITH A SUBCOMPONENT OF " &
                    "A TASK TYPE. ");

     DECLARE
          TASK TYPE TT IS
               ENTRY E;
          END TT;

          TYPE TT_ARR IS ARRAY (1..2) OF TT;

          TYPE TT_REC IS RECORD
               COMP : TT_ARR;
          END RECORD;

          GENERIC
               TYPE T IS LIMITED PRIVATE;
          PACKAGE GEN IS
               TASK TSK IS
                    ENTRY ENT(A : OUT INTEGER);
               END TSK;
          END GEN;

          INT : INTEGER;

          TASK BODY TT IS
          BEGIN
               SELECT
                    ACCEPT E;
               OR
                    TERMINATE;
               END SELECT;
          END TT;

          PACKAGE BODY GEN IS
               TASK BODY TSK IS
               BEGIN
                    DECLARE
                         TYPE ACC_T IS ACCESS T;
                         TA : ACC_T := NEW T;
                    BEGIN
                         SELECT
                              ACCEPT ENT(A : OUT INTEGER) DO
                                   A := IDENT_INT(7);
                              END;
                         OR
                              TERMINATE;
                         END SELECT;
                    END;
               END TSK;
          END GEN;

          PACKAGE GEN_TSK     IS NEW GEN(TT);
          PACKAGE GEN_TSK_SUB IS NEW GEN(TT_REC);

     BEGIN
          GEN_TSK.TSK.ENT(INT);

          IF INT /= IDENT_INT(7) THEN
               FAILED("THE WRONG VALUE WAS RETURNED BY THE TASK");
          END IF;

          INT := 0;
          GEN_TSK_SUB.TSK.ENT(INT);

          IF INT /= IDENT_INT(7) THEN
               FAILED("THE WRONG VALUE WAS RETURNED BY THE TASK, " &
                      "WITH ACTUAL PARAMETER'S BASE IS A SUB" &
                      "COMPONENT OF A TASK TYPE");
          END IF;
          RESULT;
     END;
END CC3207B;
