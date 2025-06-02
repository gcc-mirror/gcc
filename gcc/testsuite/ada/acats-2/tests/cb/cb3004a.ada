-- CB3004A.ADA

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
-- CHECK THAT WHEN AN INNER UNIT REDECLARES AN EXCEPTION NAME
--    THE HIDDEN DEFINITION IS STILL AVAILABLE FOR USE.

--  NOTE : WE ASSUME FUNCTIONS ACT LIKE PROCEDURES AND
--  THAT UNITS, BLOCKS, AND PROCEDURES ACT THE SAME
--  IN OTHER CONTEXTS (E.G. TASKS AND PACKAGES).

-- DCB 6/2/80
-- JRK 11/19/80
-- SPS 3/24/83

WITH REPORT;
PROCEDURE CB3004A IS

     USE REPORT;

     E1 : EXCEPTION;
     FLOW_COUNT : INTEGER := 0;

     PROCEDURE P1 IS
          E1, E2 : EXCEPTION;

          PROCEDURE P2 IS
               E1 : EXCEPTION;
          BEGIN
               FLOW_COUNT := FLOW_COUNT + 1;
               RAISE E1;
               FAILED("E1 EXCEPTION NOT RAISED");
          EXCEPTION
               WHEN P1.E1 =>
                    FAILED("P1.E1 EXCEPTION RAISED WHEN " &
                           "(P2)E1 EXPECTED");
               WHEN E1 =>
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE P1.E1;
                         FAILED("P1.E1 EXCEPTION NOT RAISED");
                    EXCEPTION
                         WHEN E1 =>
                              FAILED("(P2)E1 EXCEPTION RAISED WHEN" &
                                     " P1.E1 EXPECTED");
                         WHEN P1.E1 =>
                              FLOW_COUNT := FLOW_COUNT + 1;
                         WHEN OTHERS =>
                              FAILED("OTHERS RAISED WHEN P1.E1 " &
                                     "EXPECTED");
                    END;
               WHEN OTHERS =>
                    FAILED("OTHERS RAISED WHEN (P2)E1 EXPECTED");
          END P2;

          PROCEDURE P3 IS
               CONSTRAINT_ERROR : EXCEPTION;
          BEGIN
               FLOW_COUNT := FLOW_COUNT + 1;
               RAISE CONSTRAINT_ERROR;
               FAILED("CONSTRAINT_ERROR EXCEPTION NOT RAISED");
          EXCEPTION
               WHEN STANDARD.CONSTRAINT_ERROR =>
                    FAILED("STANDARD.CONSTRAINT_ERROR EXCEPTION " &
                           "RAISED WHEN " &
                           "(P3)CONSTRAINT_ERROR EXPECTED");
               WHEN CONSTRAINT_ERROR =>
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE STANDARD.CONSTRAINT_ERROR;
                         FAILED("STANDARD.CONSTRAINT_ERROR " &
                                "EXCEPTION NOT RAISED");
                    EXCEPTION
                         WHEN CONSTRAINT_ERROR =>
                              FAILED("(P3)CONSTRAINT_ERROR " &
                                     "EXCEPTION RAISED WHEN " &
                                     "STANDARD.CONSTRAINT_ERROR " &
                                     "EXPECTED");
                         WHEN STANDARD.CONSTRAINT_ERROR =>
                              FLOW_COUNT := FLOW_COUNT + 1;
                         WHEN OTHERS =>
                               FAILED("OTHERS RAISED WHEN " &
                                      "STANDARD.CONSTRAINT_ERROR " &
                                      "EXPECTED");
                    END;
               WHEN OTHERS =>
                    FAILED("OTHERS RAISED WHEN " &
                           "(P3)CONSTRAINT_ERROR EXPECTED");
          END P3;

          PROCEDURE P4 IS
               E2 : EXCEPTION;
          BEGIN
               FLOW_COUNT := FLOW_COUNT + 1;
               RAISE P1.E2;
               FAILED("P1.E2 EXCEPTION NOT RAISED");
          EXCEPTION
               WHEN E2 =>
                    FAILED("(P4).E2 RAISED WHEN P1.E2 EXPECTED");
          END P4;

     BEGIN  -- P1
          P2;
          P3;
          P4;
          FAILED("P1.E2 EXCEPTION NOT PROPAGATED FROM P4");
     EXCEPTION
          WHEN E2 =>
               FLOW_COUNT := FLOW_COUNT + 1;
          WHEN OTHERS =>
               FAILED("EXCEPTION RAISED WHERE NONE EXPECTED");
     END P1;

BEGIN
     TEST("CB3004A","CHECK THAT WHEN EXCEPTION NAMES" &
          " ARE REDECLARED THE HIDDEN DEFINITION IS STILL AVAILABLE");

     P1;

     IF FLOW_COUNT /= 8 THEN
          FAILED("INCORRECT FLOW_COUNT VALUE");
     END IF;

     RESULT;
END CB3004A;
