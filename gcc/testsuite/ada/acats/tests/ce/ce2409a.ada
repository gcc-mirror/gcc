-- CE2409A.ADA

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
--     FOR DIRECT ACCESS FILES, CHECK THAT A WRITE TO A POSITION
--     GREATER THAN THE CURRENT END POSITION CAUSES THE WRITE
--     POSITION AND THE FILE SIZE TO BE INCREMENTED.

--          1) CHECK FILES OF MODE INOUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH INOUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     ABW 08/27/82
--     SPS 11/09/82
--     SPS 03/18/83
--     EG  05/16/85
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 08/05/87  REVISED EXCEPTION HANDLING, ADDED CHECK FOR WRITE
--                   USING TO, AND MOVED OUT_FILE CASE TO CE2409B.ADA.

WITH REPORT; USE REPORT;
WITH DIRECT_IO;

PROCEDURE CE2409A IS

     PACKAGE DIR IS NEW DIRECT_IO (INTEGER);
     USE DIR;
     FILE1      : FILE_TYPE;
     INCOMPLETE : EXCEPTION;

BEGIN

     TEST ("CE2409A", "CHECK THAT WRITE POSITION AND " &
                      "SIZE ARE INCREMENTED CORRECTLY FOR " &
                      "DIR FILES OF MODE INOUT_FILE");

     BEGIN
          CREATE (FILE1, INOUT_FILE, LEGAL_FILE_NAME);
     EXCEPTION
          WHEN USE_ERROR | NAME_ERROR =>
               NOT_APPLICABLE ("CREATE WITH INOUT_FILE MODE NOT " &
                               "SUPPORTED FOR DIR FILES - 1");
               RAISE INCOMPLETE;
     END;

     DECLARE
          INT      : INTEGER := IDENT_INT (18);
          TWO_C    : COUNT := COUNT (IDENT_INT(2));
          THREE_PC : POSITIVE_COUNT
                           := POSITIVE_COUNT (IDENT_INT(3));
          FIVE_C   : COUNT := COUNT (IDENT_INT(5));
          FIVE_PC  : POSITIVE_COUNT
                           := POSITIVE_COUNT (IDENT_INT(5));
          SIX_PC  : POSITIVE_COUNT
                           := POSITIVE_COUNT (IDENT_INT(6));
     BEGIN
          WRITE (FILE1, INT);
          WRITE (FILE1, INT);
          IF INDEX (FILE1) /= THREE_PC THEN
               FAILED ("INCORRECT INDEX VALUE - 1");
          END IF;
          IF SIZE (FILE1) /= TWO_C THEN
               FAILED ("INCORRECT SIZE VALUE - 2");
          END IF;

          WRITE (FILE1, INT, FIVE_PC);
          IF INDEX (FILE1) /= SIX_PC THEN
               FAILED ("INCORRECT INDEX VALUE - 3");
          END IF;
          IF SIZE (FILE1) /= FIVE_C THEN
               FAILED ("INCORRECT SIZE VALUE - 4");
          END IF;
     END;

     BEGIN
          DELETE (FILE1);
     EXCEPTION
          WHEN USE_ERROR =>
               NULL;
     END;

     RESULT ;

EXCEPTION
     WHEN INCOMPLETE =>
          RESULT;

END CE2409A ;
