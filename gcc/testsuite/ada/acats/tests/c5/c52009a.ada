-- C52009A.ADA

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
-- CHECK THAT A RECORD VARIABLE DESIGNATED BY AN ACCESS VALUE CANNOT
-- HAVE ITS DISCRIMINANT ALTERED, EVEN BY A COMPLETE RECORD
-- ASSIGNMENT, AND EVEN THOUGH THE THE TARGET ACCESS VARIABLE IS NOT 
-- CONSTRAINED TO A SPECIFIC DISCRIMINANT VALUE.  ATTEMPTING TO
-- CHANGE THE TARGET'S DISCRIMINANT RAISES CONSTRAINT_ERROR AND LEAVES 
-- THE TARGET RECORD UNALTERED.  THIS  TEST USES STATIC DISCRIMINANT 
-- VALUES.
 
-- ASL 6/25/81
-- SPS 10/26/82

WITH REPORT;
PROCEDURE C52009A IS
 
     USE REPORT;
 
     TYPE REC (DISC : INTEGER) IS
          RECORD
               COMP : INTEGER;
          END RECORD;
 
     TYPE REC_NAME IS ACCESS REC;
 
     HR : REC_NAME := NEW REC'(5,0);
 
BEGIN
 
     TEST ("C52009A", "CANNOT CHANGE, THROUGH ASSIGNMENT, THE " &
           "(STATIC) DISCRIMINANT VALUE OF A RECORD DESIGNATED " &
           "BY AN ACCESS VALUE");
 
     BEGIN
          HR.ALL := (DISC => 5, COMP => 3);
          IF HR.ALL /= (5,3) THEN
               FAILED ("LEGAL ASSIGNMENT FAILED");
          END IF;
          HR.ALL := (DISC => 4, COMP => 2);
          FAILED ("RECORD ASSIGNED VALUE WITH DIFFERENT DISCRIMINANT " &
                  "VALUE");
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               IF HR.ALL /= (5,3) THEN
                    FAILED ("TARGET RECORD VALUE ALTERED BY " &
                            "ASSIGNMENT WITH A DIFFERENT " &
                            "DISCRIMINANT VALUE EVEN AFTER " &
                            "CONSTRAINT_ERROR RAISED");
               END IF;
          WHEN OTHERS => FAILED ("WRONG EXCEPTION");
     END;
 
     RESULT;
 
END C52009A;
