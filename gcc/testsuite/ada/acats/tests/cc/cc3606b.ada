-- CC3606B.ADA

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
--     CHECK THAT ANY CONSTRAINTS SPECIFIED FOR THE ACTUAL
--     SUBPROGRAM'S PARAMETERS ARE USED IN PLACE OF THOSE
--     ASSOCIATED WITH THE FORMAL SUBPROGRAM'S PARAMETERS
--     (INCLUDING PARAMETERS SPECIFIED WITH A FORMAL GENERIC TYPE).

-- HISTORY:
--     LDC  06/30/88 CREATED ORIGINAL TEST.
--     PWN  05/31/96 Corrected spelling problems.

WITH REPORT; USE REPORT;

PROCEDURE CC3606B IS

     SUBTYPE ONE_TO_TEN IS
          INTEGER RANGE IDENT_INT (1) .. IDENT_INT (10);
     SUBTYPE ONE_TO_FIVE IS
          INTEGER RANGE IDENT_INT (1) .. IDENT_INT (5);

BEGIN
     TEST ( "CC3606B", "CHECK THAT ANY CONSTRAINTS SPECIFIED FOR " &
                       "THE ACTUAL SUBPROGRAM'S PARAMETERS ARE USED " &
                       "IN PLACE OF THOSE ASSOCIATED WITH THE " &
                       "FORMAL SUBPROGRAM'S PARAMETERS (INCLUDING " &
                       "PARAMETERS SPECIFIED WITH A FORMAL GENERIC " &
                       "TYPE)");
     DECLARE
          GENERIC
               BRIAN : IN OUT INTEGER;
               WITH PROCEDURE PASSED_PROC(LYNN :IN OUT ONE_TO_TEN);
          PACKAGE GEN IS
          END GEN;

          DOUG : INTEGER := 10;

          PACKAGE BODY GEN IS
          BEGIN
               PASSED_PROC(BRIAN);
               FAILED("WRONG CONSTRAINTS FOR ACTUAL PARAMETER IN GEN");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("OTHER EXCEPTION WAS RAISED FOR ACTUAL " &
                           "PARAMETER");
          END GEN;

          PROCEDURE PROC(JODIE : IN OUT ONE_TO_FIVE) IS
               JOHN : ONE_TO_TEN;
          BEGIN
               JOHN := IDENT_INT(JODIE);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("EXCEPTION RAISED INSIDE PROCEDURE");
          END PROC;

          PACKAGE GEN_PCK IS NEW GEN( DOUG, PROC);

     BEGIN
          NULL;
     END;
     DECLARE
          TYPE ENUM IS (DAYTON, BEAVERCREEK, CENTERVILLE, ENGLEWOOD,
                        FAIRBORN, HUBER_HEIGHTS, KETTERING, MIAMISBURG,
                        OAKWOOD, RIVERSIDE, TROTWOOD, WEST_CARROLLTON,
                        VANDALIA);
          SUBTYPE SUB_ENUM IS ENUM RANGE  CENTERVILLE..FAIRBORN;

          GENERIC
               TYPE T_TYPE IS (<>);
               BRIAN : T_TYPE;
               WITH FUNCTION PASSED_FUNC(LYNN : T_TYPE)
                    RETURN T_TYPE;

          PACKAGE GEN_TWO IS
          END GEN_TWO;

          DOUG : ENUM := ENUM'FIRST;

          PACKAGE BODY GEN_TWO IS

          DAVE : T_TYPE;

          BEGIN
               DAVE := PASSED_FUNC(BRIAN);
               FAILED("WRONG CONSTRAINTS FOR ACTUAL PARAMETER IN " &
                      "GEN_TWO");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("OTHER EXCEPTION WAS " &
                           "RAISED FOR ACTUAL " &
                           "PARAMETER");
          END GEN_TWO;

          FUNCTION FUNC(JODIE : SUB_ENUM) RETURN SUB_ENUM IS
          BEGIN
               RETURN ENUM'VAL(IDENT_INT(ENUM'POS(JODIE)));
          EXCEPTION
               WHEN OTHERS =>
                    FAILED("EXCEPTION RAISED INSIDE PROCEDURE");
          END FUNC;

          PACKAGE GEN_PCK_TWO IS NEW GEN_TWO( ENUM, DOUG, FUNC);

     BEGIN
          RESULT;
     END;
END CC3606B;
