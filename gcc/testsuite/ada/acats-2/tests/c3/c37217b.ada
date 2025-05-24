-- C37217B.ADA

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
--     TYPE - BEFORE THE DESIGNATED TYPE'S FULL DECLARATION.

-- HISTORY:
--     DHH 08/04/88 CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;
PROCEDURE C37217B IS

     SUBTYPE SM IS INTEGER RANGE 1..10;

BEGIN  --C37217B BODY
     TEST ("C37217B", "CHECK WHETHER THE OPTIONAL COMPATIBILITY " &
                      "CHECK IS PERFORMED WHEN A DISCRIMINANT " &
                      "CONSTRAINT IS GIVEN FOR AN ACCESS TYPE - " &
                      "BEFORE THE DESIGNATED TYPE'S FULL DECLARATION");

---------------------------------------------------------------------
                                             -- INCOMPLETE DECLARATION
                                             -- UPPER LIMIT
     BEGIN  -- F
          DECLARE  -- F
               TYPE REC(D1 : INTEGER);

               TYPE PTR IS ACCESS REC;
               X : PTR(IDENT_INT(11));

               TYPE SM_REC(D : SM) IS
                    RECORD
                         NULL;
                    END RECORD;

               TYPE REC(D1 : INTEGER) IS
                    RECORD
                         INT : SM_REC(D1);
                    END RECORD;
          BEGIN
               COMMENT("OPTIONAL COMPATIBILITY CHECK NOT PERFORMED " &
                       "- UPPER");
               X := NEW REC(IDENT_INT(11));
               FAILED("CONSTRAINT ERROR NOT RAISED - UPPER");

               IF IDENT_INT(X.INT.D) /= IDENT_INT(1) THEN
                    COMMENT("IRREVELANT");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                           "VARIABLE USE - INCOMPLETE UPPER");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT("OPTIONAL COMPATIBILITY CHECK PERFORMED " &
                       "- INCOMPLETE UPPER");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                      "VARIABLE DECLARATION - INCOMPLETE UPPER");
     END;  -- F

-----------------------------------------------------------------------
                                              -- INCOMPLETE DECLARATION
                                              -- LOWER LIMIT
     BEGIN  -- A
          DECLARE  -- A
               TYPE REC(D1 : INTEGER);

               TYPE PTR IS ACCESS REC;
               X : PTR(IDENT_INT(0));

               TYPE SM_ARR IS ARRAY(SM RANGE <>) OF INTEGER;

               TYPE REC(D1 : INTEGER) IS
                    RECORD
                         INT : SM_ARR(D1 .. 2);
                    END RECORD;
          BEGIN
               COMMENT("OPTIONAL COMPATIBILITY CHECK NOT PERFORMED " &
                       "- LOWER");
               X := NEW REC'(IDENT_INT(0), INT =>
                                             (OTHERS => IDENT_INT(1)));
               FAILED("CONSTRAINT ERROR NOT RAISED - LOWER");

               IF X.INT(IDENT_INT(1)) /= IDENT_INT(1) THEN
                    COMMENT("IRREVELANT");
               END IF;
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                           "VARIABLE USE - INCOMPLETE LOWER");
          END;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               COMMENT("OPTIONAL COMPATIBILITY CHECK PERFORMED " &
                       "- INCOMPLETE LOWER");
          WHEN OTHERS =>
               FAILED("UNEXPECTED EXCEPTION RAISED IN " &
                      "VARIABLE DECLARATION - INCOMPLETE LOWER");
     END;
-----------------------------------------------------------------------
     RESULT;

END C37217B;  -- BODY
