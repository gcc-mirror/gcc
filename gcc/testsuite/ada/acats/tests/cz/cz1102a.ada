-- CZ1102A.ADA
--
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
--
-- CHECK THAT THE DYNAMIC VALUE ROUTINES OF THE REPORT PACKAGE WORK
--   CORRECTLY.

-- JRK 8/7/81
-- JRK 10/27/82
-- RLB 03/20/00 - Added checks for Integer'First and Integer'Last.

WITH REPORT;
USE REPORT;

PROCEDURE CZ1102A IS

BEGIN

     TEST ("CZ1102A", "CHECK THAT THE DYNAMIC VALUE ROUTINES OF " &
                         "THE REPORT PACKAGE WORK CORRECTLY");

     IF NOT EQUAL (0, 0) OR
        EQUAL (0, 1) OR
        NOT EQUAL (1, 1) OR
        NOT EQUAL (3, 3) OR
        NOT EQUAL (4, 4) OR
        NOT EQUAL (-1, -1) OR
        NOT EQUAL (INTEGER'FIRST, INTEGER'FIRST) OR
        NOT EQUAL (INTEGER'LAST, INTEGER'LAST) OR
        EQUAL (-1, 0) THEN
          FAILED ("'EQUAL' NOT WORKING");
     END IF;

     IF IDENT_INT (5) /= 5 THEN
          FAILED ("'IDENT_INT' NOT WORKING");
     END IF;

     IF IDENT_CHAR ('E') /= 'E' THEN
          FAILED ("'IDENT_CHAR' NOT WORKING");
     END IF;

     IF IDENT_BOOL (TRUE) /= TRUE THEN
          FAILED ("'IDENT_BOOL' NOT WORKING");
     END IF;

     IF IDENT_STR ("") /= "" OR
        IDENT_STR ("K") /= "K" OR
        IDENT_STR ("PQRS") /= "PQRS" THEN
          FAILED ("'IDENT_STR' NOT WORKING");
     END IF;

     RESULT;

END CZ1102A;
