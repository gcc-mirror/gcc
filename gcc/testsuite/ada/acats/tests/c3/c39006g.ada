-- C39006G.ADA

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
--     CHECK THAT PROGRAM_ERROR IS RAISED BY AN ATTEMPT TO CALL A
--     SUBPROGRAM WHOSE BODY IS NOT YET ELABORATED.  USE A PACKAGE
--     WITH OPTIONAL BODY, WHERE THE SUBPROGRAM IS CALLED IN THE BODY.

-- HISTORY:
--     BCB 08/01/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE C39006G IS

     PROCEDURE INIT (X : IN OUT INTEGER);

     PACKAGE P IS
     END P;

     PACKAGE BODY P IS
          X : INTEGER := IDENT_INT(5);
     BEGIN
          TEST ("C39006G", "CHECK THAT PROGRAM_ERROR IS RAISED BY " &
                           "AN ATTEMPT TO CALL A SUBPROGRAM WHOSE " &
                           "BODY IS NOT YET ELABORATED.  USE A " &
                           "PACKAGE WITH OPTIONAL BODY, WHERE THE " &
                           "SUBPROGRAM IS CALLED IN THE BODY");
          INIT(X);
          FAILED ("NO EXCEPTION RAISED");
          IF X /= IDENT_INT(10) THEN
               COMMENT ("TOTALLY IRRELEVANT");
          END IF;
          RESULT;
     EXCEPTION
          WHEN PROGRAM_ERROR =>
               RESULT;
          WHEN OTHERS =>
               FAILED ("OTHER EXCEPTION WAS RAISED");
               RESULT;
     END P;

     PROCEDURE INIT (X : IN OUT INTEGER) IS
     BEGIN
          X := IDENT_INT(10);
     END INIT;

BEGIN
     NULL;
END C39006G;
