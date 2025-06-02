-- CE2401H.ADA

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
--     CHECK THAT READ, WRITE, SET_INDEX, INDEX, SIZE, AND
--     END_OF_FILE ARE SUPPORTED FOR DIRECT FILES WITH
--     ELEMENT_TYPE UNCONSTRAINED RECORDS WITH DEFAULT DISCRIMINANTS.

--     THIS INSTANTIATION IS ALWAYS LEGAL BY AI-00037.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH INOUT_FILE MODE AND OPENING WITH IN_FILE MODE FOR
--     DIRECT FILES.

-- HISTORY:
--     TBN 05/15/86
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/10/87  ISOLATED EXCEPTIONS.

WITH REPORT;
USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2401H IS

     END_SUBTEST : EXCEPTION;

BEGIN

     TEST ("CE2401H", "CHECK THAT READ, WRITE, SET_INDEX, INDEX, " &
                       "SIZE, AND END_OF_FILE ARE SUPPORTED FOR " &
                       "DIRECT FILES WITH ELEMENT_TYPE UNCONSTRAINED " &
                       "RECORDS WITH DEFAULT DISCRIMINANTS");

     DECLARE
          TYPE REC_DEF (DISCR : INTEGER := 1) IS
               RECORD
                    ONE : INTEGER := DISCR;
                    TWO : INTEGER := 3;
                    THREE : INTEGER := 5;
                    FOUR : INTEGER := 7;
               END RECORD;
          PACKAGE DIR_REC_DEF IS NEW DIRECT_IO (REC_DEF);
          USE DIR_REC_DEF;
          FILE1 : FILE_TYPE;
          REC : REC_DEF;
          ITEM : REC_DEF;
          ONE : POSITIVE_COUNT := 1;
          TWO : POSITIVE_COUNT := 2;

     BEGIN
          BEGIN
               CREATE (FILE1, INOUT_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR | NAME_ERROR  =>
                    NOT_APPLICABLE ("CREATE WITH INOUT_FILE MODE " &
                                    "NOT SUPPORTED FOR " &
                                    "UNCONSTRAINED RECORDS WITH " &
                                    "DEFAULT DISCRIMINATES");
                    RAISE END_SUBTEST;
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED ON DIRECT " &
                            "CREATE");
                    RAISE END_SUBTEST;
          END;

          BEGIN
               WRITE (FILE1, REC);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                            "RECORD WITH DEFAULT - 1");
          END;

          BEGIN
               WRITE (FILE1, REC, TWO);
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("EXCEPTION RAISED ON WRITE FOR " &
                            "RECORD WITH DEFAULT - 2");
          END;

          BEGIN
               IF SIZE (FILE1) /= TWO THEN
                    FAILED ("SIZE FOR RECORD WITH DEFAULT");
               END IF;
               IF NOT END_OF_FILE (FILE1) THEN
                    FAILED ("WRONG END_OF_FILE VALUE FOR TYPE " &
                            "RECORD WITH DEFAULT");
               END IF;
               SET_INDEX (FILE1, ONE);
               IF INDEX (FILE1) /= ONE THEN
                    FAILED ("WRONG INDEX VALUE FOR RECORD" &
                            "WITH DEFAULT");
               END IF;
          END;

          CLOSE (FILE1);

          BEGIN
               OPEN (FILE1, IN_FILE, LEGAL_FILE_NAME);
          EXCEPTION
               WHEN USE_ERROR =>
                    NOT_APPLICABLE ("OPEN FOR IN_FILE NOT SUPPORTED");
                    RAISE END_SUBTEST;
          END;

          BEGIN
               READ (FILE1, ITEM);
               IF ITEM /= (1,1,3,5,7) THEN
                    FAILED ("WRONG VALUE READ");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("READ WITHOUT FROM FOR " &
                            "TYPE RECORD WITH DEFAULT");
          END;

          BEGIN
               ITEM := (OTHERS => 0);
               READ (FILE1, ITEM, ONE);
               IF ITEM /= (1,1,3,5,7) THEN
                    FAILED ("WRONG VALUE READ");
               END IF;
          EXCEPTION
               WHEN OTHERS =>
                    FAILED ("READ WITH FROM FOR " &
                            "TYPE RECORD WITH DEFAULT");
          END;

          BEGIN
               DELETE (FILE1);
          EXCEPTION
               WHEN USE_ERROR =>
                    NULL;
          END;

     EXCEPTION
          WHEN END_SUBTEST =>
               NULL;
     END;

     RESULT;

END CE2401H;
