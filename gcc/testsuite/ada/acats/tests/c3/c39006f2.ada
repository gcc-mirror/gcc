-- C39006F2.ADA

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
--     CHECK THAT NO PROGRAM_ERROR IS RAISED IF A SUBPROGRAM'S BODY HAS
--     BEEN ELABORATED BEFORE IT IS CALLED.  CHECK THE FOLLOWING:
--        B) FOR A SUBPROGRAM LIBRARY UNIT USED IN ANOTHER UNIT, NO
--           PROGRAM_ERROR IS RAISED IF PRAGMA ELABORATE NAMES THE
--           SUBPROGRAM.

--     THIS LIBRARY PACKAGE BODY IS USED BY C39006F3M.ADA.

-- HISTORY:
--     TBN  08/22/86  CREATED ORIGINAL TEST.
--     BCB  03/29/90  CORRECTED HEADER.  CHANGED TEST NAME IN CALL
--                    TO 'TEST'.
--     PWN  05/25/94  ADDED A PROCEDURE TO KEEP PACKAGE BODIES LEGAL.

WITH C39006F0;
WITH REPORT; USE REPORT;
PRAGMA ELABORATE (C39006F0, REPORT);

PACKAGE BODY C39006F1 IS

     PROCEDURE REQUIRE_BODY IS
     BEGIN
          NULL;
     END;

BEGIN
     TEST ("C39006F", "CHECK THAT NO PROGRAM_ERROR IS RAISED IF A " &
                      "SUBPROGRAM'S BODY HAS BEEN ELABORATED " &
                      "BEFORE IT IS CALLED, WHEN A SUBPROGRAM " &
                      "LIBRARY UNIT IS USED IN ANOTHER UNIT AND " &
                      "PRAGMA ELABORATE IS USED");
     BEGIN
          DECLARE
               VAR1 : INTEGER := C39006F0 (IDENT_INT(1));
          BEGIN
               IF VAR1 /= IDENT_INT(1) THEN
                    FAILED ("INCORRECT RESULTS - 1");
               END IF;
          END;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               FAILED ("PROGRAM_ERROR RAISED - 1");
          WHEN OTHERS =>
               FAILED ("UNEXPECTED EXCEPTION RAISED - 1");
     END;

     DECLARE
          VAR2 : INTEGER := 1;

          PROCEDURE CHECK (B : IN OUT INTEGER) IS
          BEGIN
               B := C39006F0 (IDENT_INT(2));
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    FAILED ("PROGRAM_ERROR RAISED - 2");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION RAISED - 2");
          END CHECK;
     BEGIN
          CHECK (VAR2);
          IF VAR2 /= IDENT_INT(2) THEN
               FAILED ("INCORRECT RESULTS - 2");
          END IF;
     END;

     DECLARE
          PACKAGE P IS
               VAR3 : INTEGER;
          END P;

          PACKAGE BODY P IS
          BEGIN
               VAR3 := C39006F0 (IDENT_INT(3));
               IF VAR3 /= IDENT_INT(3) THEN
                    FAILED ("INCORRECT RESULTS - 3");
               END IF;
          EXCEPTION
               WHEN PROGRAM_ERROR =>
                    FAILED ("PROGRAM_ERROR RAISED - 3");
               WHEN OTHERS =>
                    FAILED ("UNEXPECTED EXCEPTION - 3");
          END P;
     BEGIN
          NULL;
     END;

     DECLARE
          GENERIC
               VAR4 : INTEGER := 1;
          PACKAGE Q IS
               TYPE ARRAY_TYP1 IS ARRAY (1 .. VAR4) OF INTEGER;
               ARRAY_1 : ARRAY_TYP1;
          END Q;

          PACKAGE NEW_Q IS NEW Q (C39006F0 (IDENT_INT(4)));

          USE NEW_Q;

     BEGIN
          IF ARRAY_1'LAST /= IDENT_INT(4) THEN
               FAILED ("INCORRECT RESULTS - 4");
          END IF;
     END;

END C39006F1;
