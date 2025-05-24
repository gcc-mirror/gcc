-- CB1005A.ADA

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
-- CHECK THAT EXCEPTIONS DECLARED IN GENERIC PACKAGES AND PROCEDURES ARE
-- CONSIDERED DISTINCT FOR EACH INSTANTIATION.

-- CHECK THAT AN EXCEPTION NAME DECLARED IN A GENERIC PACKAGE
-- INSTANTIATION IN A RECURSIVE PROCEDURE DENOTES THE SAME ENTITY
-- EVEN WHEN THE INSTANTIATION IS ELABORATED MORE THAN ONCE BECAUSE
-- OF RECURSIVE CALLS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X
-- ***                                                                 -- 9X

-- TBN  9/23/86
-- MRM  03/30/93  REMOVED NUMERIC_ERROR FOR 9X COMPATIBILITY

WITH REPORT; USE REPORT;
PROCEDURE CB1005A IS

     PROCEDURE PROP;

     GENERIC
          PACKAGE PAC IS
               EXC : EXCEPTION;
          END PAC;

     GENERIC
          PROCEDURE PROC (INST_AGAIN : BOOLEAN);

     PROCEDURE PROC (INST_AGAIN : BOOLEAN) IS
          EXC : EXCEPTION;
     BEGIN
          IF INST_AGAIN THEN
               BEGIN
                    PROP;
                    FAILED ("EXCEPTION WAS NOT PROPAGATED - 9");
               EXCEPTION
                    WHEN EXC =>
                         FAILED ("EXCEPTION NOT DISTINCT - 10");
                    WHEN PROGRAM_ERROR | STORAGE_ERROR |
                         TASKING_ERROR | CONSTRAINT_ERROR =>
                         FAILED ("WRONG EXCEPTION PROPAGATED - 11");
                    WHEN OTHERS =>
                         NULL;
               END;
          ELSE
               RAISE EXC;
          END IF;
     END PROC;

     PROCEDURE RAISE_EXC (CALL_AGAIN : BOOLEAN) IS
          PACKAGE PAC3 IS NEW PAC;
     BEGIN
          IF CALL_AGAIN THEN
               BEGIN
                    RAISE_EXC (FALSE);
                    FAILED ("EXCEPTION WAS NOT PROPAGATED - 12");
               EXCEPTION
                    WHEN PAC3.EXC =>
                         NULL;
               END;
          ELSE
               RAISE PAC3.EXC;
          END IF;
     END RAISE_EXC;

     PROCEDURE PROP IS
          PROCEDURE PROC2 IS NEW PROC;
     BEGIN
          PROC2 (FALSE);
     END PROP;

BEGIN
     TEST ("CB1005A", "CHECK THAT EXCEPTIONS DECLARED IN GENERIC " &
                      "PACKAGES AND PROCEDURES ARE CONSIDERED " &
                      "DISTINCT FOR EACH INSTANTIATION");

     -------------------------------------------------------------------
     DECLARE
          PACKAGE PAC1 IS NEW PAC;
          PACKAGE PAC2 IS NEW PAC;
          PAC1_EXC_FOUND : BOOLEAN := FALSE;
     BEGIN
          BEGIN
               IF EQUAL (3, 3) THEN
                    RAISE PAC2.EXC;
               END IF;
               FAILED ("EXCEPTION WAS NOT RAISED - 1");

          EXCEPTION
               WHEN PAC1.EXC =>
                    FAILED ("PACKAGE EXCEPTIONS NOT DISTINCT - 2");
                    PAC1_EXC_FOUND := TRUE;
          END;
          IF NOT PAC1_EXC_FOUND THEN
               FAILED ("EXCEPTION WAS NOT PROPAGATED - 3");
          END IF;

     EXCEPTION
          WHEN PAC1.EXC =>
               FAILED ("PACKAGE EXCEPTIONS NOT DISTINCT - 4");
          WHEN PAC2.EXC =>
               BEGIN
                    IF EQUAL (3, 3) THEN
                         RAISE PAC1.EXC;
                    END IF;
                    FAILED ("EXCEPTION WAS NOT RAISED - 5");

               EXCEPTION
                    WHEN PAC2.EXC =>
                         FAILED ("PACKAGE EXCEPTIONS NOT DISTINCT - 6");
                    WHEN PAC1.EXC =>
                         NULL;
                    WHEN OTHERS =>
                         FAILED ("UNKNOWN EXCEPTION RAISED - 7");
               END;
          WHEN OTHERS =>
               FAILED ("UNKNOWN EXCEPTION RAISED - 8");
     END;

     -------------------------------------------------------------------
     DECLARE
          PROCEDURE PROC1 IS NEW PROC;
     BEGIN
          PROC1 (TRUE);
     END;

     -------------------------------------------------------------------
     BEGIN
          RAISE_EXC (TRUE);

     EXCEPTION
          WHEN OTHERS =>
               FAILED ("EXCEPTIONS ARE DISTINCT FOR RECURSION - 13");
     END;

     -------------------------------------------------------------------

     RESULT;
END CB1005A;
