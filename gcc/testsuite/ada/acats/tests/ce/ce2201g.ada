-- CE2201G.ADA

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
--      CHECK THAT READ, WRITE, AND END_OF_FILE ARE SUPPORTED
--      FOR SEQUENTIAL FILES WITH VARIANT RECORDS WITH DEFAULT
--      DISCRIMINANTS.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     SEQUENTIAL FILES.

-- HISTORY:
--     TBN 05/15/86
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/03/87  REMOVED DEPENDENCE OF RESET AND CREATED EXTERNAL
--                   FILES RATHER THAN TEMPORARY FILES.

WITH REPORT; USE REPORT;
WITH SEQUENTIAL_IO;

PROCEDURE CE2201G IS

BEGIN

     TEST ("CE2201G", "CHECK THAT READ, WRITE, AND END_OF_FILE " &
                      "ARE SUPPORTED FOR SEQUENTIAL FILES WITH " &
                      "UNCONSTRAINED VARIANT RECORD TYPES WITH " &
                      "DEFAULT DISCRIMINANTS.");

     DECLARE
          TYPE VAR_REC (DISCR : BOOLEAN := TRUE) IS
               RECORD
                    CASE DISCR IS
                         WHEN TRUE =>
                              A : INTEGER;
                         WHEN FALSE =>
                              B : STRING (1..20);
                    END CASE;
               END RECORD;

          PACKAGE SEQ_VAR_REC IS NEW SEQUENTIAL_IO (VAR_REC);
          USE SEQ_VAR_REC;

          FILE_VAR_REC : FILE_TYPE;
          INCOMPLETE : EXCEPTION;
          ITEM_TRUE : VAR_REC(TRUE);     -- CONSTRAINED
          ITEM      : VAR_REC;           -- UNCONSTRAINED

     BEGIN
          BEGIN
               CREATE (FILE_VAR_REC, OUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR =>
                    NOT_APPLICABLE ("CREATE OF SEQUENTIAL FILE WITH " &
                                    "MODE OUT_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          WRITE (FILE_VAR_REC, (TRUE, -5));
          WRITE (FILE_VAR_REC, (FALSE, (1..20 => 'B')));
          CLOSE (FILE_VAR_REC);

          BEGIN
               OPEN (FILE_VAR_REC, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN OF SEQUENTIAL FILE WITH " &
                                    "MODE IN_FILE NOT SUPPORTED");
                    RAISE INCOMPLETE;
          END;

          IF END_OF_FILE (FILE_VAR_REC) THEN
               FAILED ("WRONG END_OF_FILE VALUE FOR RECORD" &
                       "WITH DISCRIMINANT");
          END IF;

          BEGIN
               READ (FILE_VAR_REC, ITEM_TRUE);

               IF ITEM_TRUE /= (TRUE, IDENT_INT(-5)) THEN
                    FAILED ("READ WRONG VALUE - 1");
               END IF;

               IF END_OF_FILE (FILE_VAR_REC) THEN
                    FAILED ("PREMATURE END OF FILE");
               END IF;

               READ (FILE_VAR_REC, ITEM);

               IF ITEM /= (FALSE, (1..IDENT_INT(20) => 'B')) THEN
                    FAILED ("READ WRONG VALUE - 2");
               END IF;

               IF NOT END_OF_FILE(FILE_VAR_REC) THEN
                    FAILED ("NOT AT END OF FILE");
               END IF;

          END;

          BEGIN
               DELETE (FILE_VAR_REC);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN INCOMPLETE =>
               NULL;

     END;

     RESULT;

END CE2201G;
