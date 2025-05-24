-- C37411A.ADA

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
--     CHECK THAT THE OPERATIONS OF ASSIGNMENT, COMPARISON, MEMBERSHIP
--     TESTS, QUALIFICATION, TYPE CONVERSION, 'BASE, 'SIZE AND 'ADDRESS,
--     ARE DEFINED FOR NULL RECORDS.

-- HISTORY:
--     DHH 03/04/88 CREATED ORIGINAL TEST.
--     PWN 11/30/94 REMOVED 'BASE USE ILLEGAL IN ADA 9X.

WITH SYSTEM; USE SYSTEM;
WITH REPORT; USE REPORT;
PROCEDURE C37411A IS
     TYPE S IS
          RECORD
               NULL;
          END RECORD;

     SUBTYPE SS IS S;

     U,V,W : S;
     X : SS;

BEGIN

     TEST("C37411A", "CHECK THAT THE OPERATIONS OF ASSIGNMENT, " &
                     "COMPARISON, MEMBERSHIP TESTS, QUALIFICATION, " &
                     "TYPE CONVERSION, 'BASE, 'SIZE AND 'ADDRESS, " &
                     "ARE DEFINED FOR NULL RECORDS");
     U := W;
     IF U /= W THEN
          FAILED("EQUALITY/ASSIGNMENT DOES NOT PERFORM CORRECTLY");
     END IF;

     IF V NOT IN S THEN
          FAILED("MEMBERSHIP DOES NOT PERFORM CORRECTLY");
     END IF;

     IF X /= SS(V) THEN
          FAILED("TYPE CONVERSION DOES NOT PERFORM CORRECTLY");
     END IF;

     IF S'(U) /= S'(W) THEN
          FAILED("QUALIFIED EXPRESSION DOES NOT PERFORM CORRECTLY");
     END IF;

     IF X'SIZE /= V'SIZE THEN
          FAILED("'BASE'SIZE DOES NOT PERFORM CORRECTLY WHEN PREFIX " &
                 "IS AN OBJECT");
     END IF;

     IF X'ADDRESS = V'ADDRESS THEN
          COMMENT("NULL RECORDS HAVE THE SAME ADDRESS");
     ELSE
          COMMENT("NULL RECORDS DO NOT HAVE THE SAME ADDRESS");
     END IF;

     RESULT;
END C37411A;
