-- C87B08A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE RULE THAT:
--
-- FOR EACH REAL TYPE, THERE EXISTS AN IMPLICIT CONVERSION THAT
-- CONVERTS A UNIVERSAL REAL VALUE INTO THE CORRESPONDING VALUE 
-- OF THE REAL TYPE. THIS TEST USES LITERALS AS UNIVERSAL REAL
-- VALUES.
  
-- TRH  16 AUG 82
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B08A IS
 
     TYPE FIXED IS DELTA 0.1 RANGE -2.0 .. 2.0;
     TYPE FLT   IS DIGITS  2 RANGE -2.0 .. 2.0;
     TYPE FLAG  IS (PASS, FAIL);
 
     GENERIC
          TYPE T IS PRIVATE;
          STAT : IN FLAG;
     PROCEDURE P1 (X : T);
  
     PROCEDURE P1 (X : T) IS
     BEGIN
          IF STAT = FAIL THEN
               FAILED ("INCORRECT IMPLICIT CONVERSION FROM UNIVERSAL" &
                       " REAL VALUES TO REAL TYPE VALUES");
          END IF;
     END P1;
 
     PROCEDURE P IS NEW P1 (INTEGER,   FAIL);
     PROCEDURE P IS NEW P1 (FLT,       PASS);
     PROCEDURE Q IS NEW P1 (FIXED,     PASS);
     PROCEDURE Q IS NEW P1 (BOOLEAN,   FAIL);
     PROCEDURE Q IS NEW P1 (CHARACTER, FAIL);
 
BEGIN
     TEST ("C87B08A","IMPLICIT CONVERSION OF UNIVERSAL REAL " &
           "VALUES TO REAL VALUES EXISTS FOR ANY REAL TYPE");
    
     P (0.0);
     P (1.0 + 1.0);
     Q (1.0);
     Q (1.0 - 1.0);
 
     RESULT;
END C87B08A;
