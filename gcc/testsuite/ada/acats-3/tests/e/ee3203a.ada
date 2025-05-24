-- EE3203A.ADA

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
--     CHECK THAT SET_INPUT AND SET_OUTPUT CAN BE USED, AND THAT THEY
--     DO NOT REDEFINE OR CLOSE THE CORRESPONDING STANDARD FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- PASS/FAIL CRITERIA:
--     THIS TEST IS PASSED IF IT EXECUTES AND THE STANDARD OUTPUT FILE
--     CONTAINS THE LINE "INITIAL TEXT OF STANDARD_OUTPUT".

-- HISTORY:
--     ABW 08/25/82
--     SPS 11/19/82
--     VKG 02/15/83
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 08/19/87  CORRECTED EXCEPTION HANDLING, REMOVED DEPENDENCE
--                   ON RESET, AND ADDED CHECKS FOR USE_ERROR ON DELETE.

WITH REPORT;
USE REPORT;
WITH TEXT_IO;
USE TEXT_IO;
WITH CHECK_FILE;

PROCEDURE EE3203A IS

     INCOMPLETE : EXCEPTION;
     FILE_IN, FILE_OUT : FILE_TYPE;
     LST : NATURAL;
     IN_STR : STRING (1 .. 50);

BEGIN

     TEST ("EE3203A", "CHECK THAT SET_INPUT AND SET_OUTPUT " &
                      "CAN BE USED, AND THAT CORRESPONDING " &
                      "STANDARD FILES ARE UNCHANGED");

     BEGIN
          CREATE (FILE_IN, OUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE WITH " &
                               "OUT_FILE MODE - 1");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE - 1");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     BEGIN
          CREATE (FILE_OUT, OUT_FILE, LEGAL_FILE_NAME(2));
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT CREATE WITH " &
                               "OUT_FILE MODE - 2");
               RAISE INCOMPLETE;
          WHEN NAME_ERROR =>
               NOT_APPLICABLE ("NAME_ERROR RAISED ON TEXT CREATE " &
                               "WITH OUT_FILE MODE - 2");
               RAISE INCOMPLETE;
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED ON TEXT CREATE");
               RAISE INCOMPLETE;
     END;

     PUT (FILE_IN, "INITIAL TEXT OF FILE_IN");
     PUT (FILE_OUT, "INITIAL TEXT OF FILE_OUT");
     PUT ("INITIAL TEXT OF STANDARD_OUTPUT");

     CLOSE (FILE_IN);

     BEGIN
          OPEN (FILE_IN, IN_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR =>
               NOT_APPLICABLE ("USE_ERROR RAISED ON TEXT OPEN WITH " &
                               "IN_FILE MODE");
               RAISE INCOMPLETE;
     END;

     SET_INPUT (FILE_IN);
     SET_OUTPUT (FILE_OUT);

     IF NOT IS_OPEN (STANDARD_INPUT) THEN
          FAILED ("STANDARD_INPUT NOT OPEN");
     END IF;

     IF NOT IS_OPEN (FILE_IN) THEN
          FAILED ("FILE_IN NOT OPEN");
     END IF;

     IF NOT IS_OPEN (STANDARD_OUTPUT) THEN
          FAILED ("STANDARD_OUTPUT NOT OPEN");
     END IF;

     IF NOT IS_OPEN (FILE_OUT) THEN
          FAILED ("FILE_OUT NOT OPEN");
     END IF;

     NEW_LINE;
     PUT ("SECOND LINE OF OUTPUT");

     GET_LINE (IN_STR, LST);
     IF IN_STR (1 .. LST) /= "INITIAL TEXT OF FILE_IN" THEN
          FAILED ("DEFAULT INPUT INCORRECT");
     END IF;

     CHECK_FILE (FILE_IN, "INITIAL TEXT OF FILE_IN#@%");
     SET_OUTPUT (FILE => STANDARD_OUTPUT);
     SET_INPUT (FILE => STANDARD_INPUT);
     CHECK_FILE (FILE_OUT, "INITIAL TEXT OF FILE_OUT#" &
                           "SECOND LINE OF OUTPUT#@%");

     SPECIAL_ACTION ("THE STANDARD OUTPUT FILE SHOULD CONTAIN " &
                     "THE LINE : INITIAL TEXT OF STANDARD_OUTPUT");

     BEGIN
          DELETE (FILE_IN);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     BEGIN
          DELETE (FILE_OUT);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END EE3203A;
