-- C39006C0M.ADA

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
-- CHECK THAT PROGRAM_ERROR IS RAISED IF AN ATTEMPT IS MADE TO CALL A
-- SUBPROGRAM WHOSE BODY HAS NOT YET BEEN ELABORATED.  CHECK THE
-- FOLLOWING:
--     D) THE SUBPROGRAM IS CALLED DURING ELABORATION OF AN OPTIONAL
--        PACKAGE BODY SUBUNIT THAT IS IN C39006C1.ADA.

-- SEPARATE FILES ARE:
--     C39006C0M     THE MAIN PROCEDURE.
--     C39006C1      A SUBUNIT PACKAGE BODY.

-- TBN  8/19/86
-- LDC  5/26/88   CHANGED TEST NAME PARAMETER FROM C39006C0M TO
--                C39006C IN THE TEST CALL.

WITH REPORT; USE REPORT;
PROCEDURE C39006C0M IS

     PACKAGE CALL_TEST_FIRST IS
     END CALL_TEST_FIRST;

     PACKAGE BODY CALL_TEST_FIRST IS
     BEGIN
          TEST ("C39006C", "CHECK THAT PROGRAM_ERROR IS RAISED IF " &
                             "THE SUBPROGRAM WHOSE BODY HAS NOT BEEN " &
                             "ELABORATED IS CALLED DURING " &
                             "ELABORATION OF AN OPTIONAL PACKAGE " &
                             "BODY SUBUNIT");
     END CALL_TEST_FIRST;

     PROCEDURE ADD1 (A : IN OUT INTEGER);

     PACKAGE C39006C1 IS
          VAR : INTEGER := IDENT_INT(1);
     END C39006C1;

     PACKAGE BODY C39006C1 IS SEPARATE;

     PROCEDURE ADD1 (A : IN OUT INTEGER) IS
     BEGIN
          A := A + IDENT_INT(1);
     END ADD1;

BEGIN
     RESULT;
END C39006C0M;
