-- C35703A.ADA

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
-- CHECK THAT 'FIRST AND 'LAST EXIST AND CAN BE ASSIGNED.  CHECK THAT
-- 'FIRST IS LESS THAN OR EQUAL TO 'LAST.

-- BAW 5 SEPT 80
-- R.WILLIAMS 8/21/86    ADDED A TYPE DECLARED WITHOUT A RANGE
--                       CONSTRAINT.  RENAMED TO -B.  ADDED EXCEPTION
--                       HANDLERS.
-- GMT 6/29/87           MOVED THE CALL TO  REPORT.TEST INTO A NEWLY
--                       CREATED PACKAGE NAMED SHOW_TEST_HEADER.


WITH REPORT; USE REPORT;
PROCEDURE C35703A IS

     TYPE REAL1 IS DIGITS 2 RANGE 0.25..0.5;
     TYPE REAL2 IS DIGITS 3;

     PACKAGE  SHOW_TEST_HEADER  IS
              -- PURPOSE OF THIS PACKAGE:
              -- WE WANT THE TEST HEADER INFORMATION TO BE
              -- PRINTED  BEFORE  ANY OF THE  PASS/FAIL  MESSAGES.
     END SHOW_TEST_HEADER;

     PACKAGE  BODY  SHOW_TEST_HEADER  IS
     BEGIN
          TEST( "C35703A",
                "CHECK THAT FIRST AND LAST CAN BE ASSIGNED " &
                "AND THAT FIRST <= LAST" );
     END SHOW_TEST_HEADER;

     PACKAGE XPKG IS
          X : REAL1;
     END XPKG;

     PACKAGE BODY XPKG IS
     BEGIN
          X := REAL1'FIRST;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " &
                        "REAL1'FIRST" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY ASSIGNMENT OF " &
                        "REAL1'FIRST" );
     END XPKG;

     PACKAGE YPKG IS
          Y : REAL1;
     END YPKG;

     PACKAGE BODY YPKG IS
     BEGIN
          Y := REAL1'LAST;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " &
                        "REAL1'LAST" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY ASSIGNMENT OF " &
                        "REAL1'LAST" );
     END YPKG;

     PACKAGE APKG IS
          A : REAL2;
     END APKG;

     PACKAGE BODY APKG IS
     BEGIN
          A := REAL2'FIRST;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " &
                        "REAL2'FIRST" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY ASSIGNMENT OF " &
                        "REAL2'FIRST" );
     END APKG;

     PACKAGE BPKG IS
          B : REAL2;
     END BPKG;

     PACKAGE BODY BPKG IS
     BEGIN
          B := REAL2'LAST;
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED ( "CONSTRAINT_ERROR RAISED BY ASSIGNMENT OF " &
                        "REAL2'LAST" );
          WHEN OTHERS =>
               FAILED ( "OTHER EXCEPTION RAISED BY ASSIGNMENT OF " &
                        "REAL2'LAST" );
     END BPKG;


BEGIN

     DECLARE
          USE XPKG;
          USE YPKG;
     BEGIN
          IF X > Y THEN
               FAILED ( "REAL1'FIRST IS GREATER THAN REAL1'LAST" );
          END IF;
     END;

     DECLARE
          USE APKG;
          USE BPKG;
     BEGIN
          IF A > B THEN
               FAILED ( "REAL2'FIRST IS GREATER THEN REAL2'LAST" );
          END IF;
     END;

     RESULT;

END C35703A;
