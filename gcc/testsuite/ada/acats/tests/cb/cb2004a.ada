-- CB2004A.ADA

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
-- CHECK THAT A PREDEFINED OR A PROGRAMMER DEFINED EXCEPTION
--    RAISED SEVERAL LEVELS INSIDE A HIERARCHY OF NESTED BLOCKS
--    CAN BE SUCCESSFULLY HANDLED IN AN OUTER BLOCK.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- DCB 5/12/80
-- JRK 11/17/80
-- SPS 11/2/82
-- MRM 03/30/93   REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT;
PROCEDURE CB2004A IS

     USE REPORT;

     FLOW_COUNT : INTEGER := 0;

     E1, E2, E3 : EXCEPTION;

BEGIN
     TEST("CB2004A","CHECK THAT EXCEPTIONS RAISED INSIDE NESTED " &
          "BLOCKS CAN BE HANDLED IN OUTER BLOCKS");

     BEGIN

          -- PROGRAMMER-DEFINED EXCEPTION, SINGLE EXCEPTON_CHOICE.

          BEGIN
               BEGIN
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE E1;
                         FAILED("PROGRAMMER-DEFINED EXCEPTION " &
                                "NOT RAISED  #1");

                    EXCEPTION
                         WHEN E2 | E3 =>
                              FAILED("WRONG PROGRAMMER-" &
                                     "DEFINED EXCEPTION HANDLED   #1");
                    END;

               EXCEPTION
                    WHEN CONSTRAINT_ERROR |
                         PROGRAM_ERROR | STORAGE_ERROR |
                         TASKING_ERROR | E2 | E3     =>
                         FAILED("WRONG  " &
                                "EXCEPTION HANDLED   #1");
               END;

          EXCEPTION
               WHEN E1 =>
                    FLOW_COUNT := FLOW_COUNT + 1;
          END;

          -- PROGRAMMER-DEFINED EXCEPTION, MULTIPLE EXCEPTION_CHOICES.

          BEGIN
               BEGIN
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE E2;
                         FAILED("PROGRAMMER-DEFINED EXCEPTION " &
                                "NOT RAISED  #2");

                    EXCEPTION
                         WHEN E1 | E3 =>
                              FAILED("WRONG PROGRAMMER-" &
                                     "DEFINED EXCEPTION HANDLED   #2");
                    END;

               EXCEPTION
                    WHEN CONSTRAINT_ERROR |
                         PROGRAM_ERROR | STORAGE_ERROR |
                         TASKING_ERROR | E1 | E3     =>
                         FAILED("WRONG  " &
                                "EXCEPTION HANDLED   #2");
               END;

          EXCEPTION
               WHEN E3 =>
                    FAILED("WRONG EXCEPTION HANDLED  #2A");
               WHEN E1 | E2 | CONSTRAINT_ERROR =>
                    FLOW_COUNT := FLOW_COUNT + 1;
          END;

          -- PROGRAMMER-DEFINED EXCEPTION, 'OTHERS' CHOICE.

          BEGIN
               BEGIN
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE E1;
                         FAILED("PROGRAMMER-DEFINED EXCEPTION " &
                                "NOT RAISED  #3");

                    EXCEPTION
                         WHEN E2 | E3 =>
                              FAILED("WRONG PROGRAMMER-" &
                                     "DEFINED EXCEPTION HANDLED   #3");
                    END;

               EXCEPTION
                    WHEN CONSTRAINT_ERROR |
                         PROGRAM_ERROR | STORAGE_ERROR |
                         TASKING_ERROR | E2 | E3   =>
                         FAILED("WRONG " &
                                "EXCEPTION HANDLED   #3");
               END;

          EXCEPTION
               WHEN E2 | CONSTRAINT_ERROR =>
                    FAILED("WRONG EXCEPTION HANDLED  #3A");
               WHEN OTHERS =>
                    FLOW_COUNT := FLOW_COUNT + 1;
          END;

          -- PREDEFINED EXCEPTION, SINGLE EXCEPTION_CHOICE.

          BEGIN
               BEGIN
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE CONSTRAINT_ERROR;
                         FAILED("PREDEFINED EXCEPTION NOT RAISED  #4");

                    EXCEPTION
                         WHEN E1 | E2 | E3 =>
                              FAILED("WRONG " &
                                     "EXCEPTION HANDLED   #4");
                    END;

               EXCEPTION
                    WHEN PROGRAM_ERROR | STORAGE_ERROR |
                         TASKING_ERROR     =>
                         FAILED("WRONG PREDEFINED " &
                                "EXCEPTION HANDLED   #4");
               END;

          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    FLOW_COUNT := FLOW_COUNT + 1;
          END;

          -- PREDEFINED EXCEPTION, MULTIPLE EXCEPTION_CHOICES.

          BEGIN
               BEGIN
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE CONSTRAINT_ERROR;
                         FAILED("PREDEFINED EXCEPTION NOT RAISED  #5");

                    EXCEPTION
                         WHEN E1 | E2 | E3 =>
                              FAILED("WRONG " &
                                     "EXCEPTION HANDLED   #5");
                    END;

               EXCEPTION
                    WHEN PROGRAM_ERROR | 
                         STORAGE_ERROR | TASKING_ERROR     =>
                         FAILED("WRONG PREDEFINED " &
                                "EXCEPTION HANDLED   #5");
               END;

          EXCEPTION
               WHEN E1 | E2 =>
                    FAILED("WRONG EXCEPTION HANDLED  #5A");
               WHEN CONSTRAINT_ERROR | E3 =>
                    FLOW_COUNT := FLOW_COUNT + 1;
          END;

          -- PREDEFINED EXCEPTION, 'OTHERS' CHOICE.

          BEGIN
               BEGIN
                    BEGIN
                         FLOW_COUNT := FLOW_COUNT + 1;
                         RAISE CONSTRAINT_ERROR;
                         FAILED("PREDEFINED EXCEPTION NOT RAISED  #6");

                    EXCEPTION
                         WHEN E1 | E2 | E3 =>
                              FAILED("WRONG " &
                                     " EXCEPTION HANDLED   #6");
                    END;

               EXCEPTION
                    WHEN PROGRAM_ERROR | STORAGE_ERROR |
                         TASKING_ERROR     =>
                         FAILED("WRONG PREDEFINED " &
                                "EXCEPTION HANDLED   #6");
               END;

          EXCEPTION
               WHEN E1 =>
                    FAILED("WRONG EXCEPTION HANDLED  #6A");
               WHEN OTHERS =>
                    FLOW_COUNT := FLOW_COUNT + 1;
          END;

     EXCEPTION
          WHEN E1 | E2 | E3 =>
               FAILED("PROGRAMMER-DEFINED EXCEPTION HANDLED IN" &
                      "WRONG SCOPE");
          WHEN CONSTRAINT_ERROR =>
               FAILED("CONSTRAINT_ERROR HANDLED IN WRONG SCOPE");
          WHEN OTHERS =>
               FAILED("OTHER EXCEPTIONS HANDLED IN WRONG SCOPE");
     END;

     IF FLOW_COUNT /= 12 THEN
          FAILED("INCORRECT FLOW_COUNT VALUE");
     END IF;

     RESULT;
END CB2004A;
