-- C37217A.ADA

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
--     CHECK WHETHER THE OPTIONAL COMPATIBILITY CHECK IS
--     PERFORMED WHEN A DISCRIMINANT CONSTRAINT IS GIVEN FOR AN ACCESS
--     TYPE - AFTER THE TYPE'S FULL DECLARATION.

-- HISTORY:
--     DHH 02/05/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C37217A IS

     SUBTYPE SM IS INTEGER RANGE 1..10;

BEGIN  --C37217A BODY
     TEST ("C37217A", "CHECK WHETHER THE OPTIONAL COMPATIBILITY " &
                      "CHECK IS PERFORMED WHEN A DISCRIMINANT " &
                      "CONSTRAINT IS GIVEN FOR AN ACCESS TYPE " &
                      "- AFTER THE TYPE'S FULL DECLARATION");

                                         -- CHECK FULL DECLARATION
                                         -- LOWER LIMIT
     BEGIN
          DECLARE

               TYPE SM_REC(D : SM) IS
                    RECORD
                         NULL;
                    END RECORD;

               TYPE REC(D1 : INTEGER) IS
                    RECORD
                         INT : SM_REC(D1);
                    END RECORD;

               TYPE PTR IS ACCESS REC;

               Y : PTR(IDENT_INT(0));           -- OPTIONAL EXCEPTION.
          BEGIN
               COMMENT("OPTIONAL COMBATIBILITY CHECK NOT PERFORMED " &
                       "- LOWER");
               Y := NEW REC(IDENT_INT(0));      -- MANDATORY EXCEPTION.
               FAILED("CONSTRAINT ERROR NOT RAISED");

               IF IDENT_INT(Y.INT.D) /= IDENT_INT(-1) THEN
                    COMMENT ("IRRELEVANT");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                           "VARIABLE ALLOCATION - LOWER");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT("OPTIONAL CONSTRAINT ERROR RAISED - LOWER");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                      "VARIABLE DECLARATION - LOWER");
     END;
---------------------------------------------------------------------
                                         -- CHECK FULL DECLARATION
                                         -- UPPER LIMIT
     BEGIN
          DECLARE
               TYPE SM_ARR IS ARRAY(SM RANGE <>) OF INTEGER;

               TYPE REC(D1 : INTEGER) IS
                    RECORD
                         INT : SM_ARR(1 .. D1);
                    END RECORD;

               TYPE PTR IS ACCESS REC;

               Y : PTR(IDENT_INT(11));           -- OPTIONAL EXCEPTION.
          BEGIN
               COMMENT("OPTIONAL COMBATIBILITY CHECK NOT PERFORMED " &
                       "- UPPER");
               Y := NEW REC'(IDENT_INT(11),     -- MANDATORY EXCEPTION.
                                      INT => (OTHERS => IDENT_INT(0)));
               FAILED("CONSTRAINT ERROR NOT RAISED");

               IF IDENT_INT(Y.INT(IDENT_INT(1))) /= 11 THEN
                    COMMENT ("IRRELEVANT");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                           "VARIABLE ALLOCATION - UPPER");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT("OPTIONAL COMPATIBILITY CHECK PERFORMED " &
                       "- UPPER");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                      "VARIABLE DECLARATION - UPPER");
     END;

     RESULT;

END C37217A;  -- BODY
