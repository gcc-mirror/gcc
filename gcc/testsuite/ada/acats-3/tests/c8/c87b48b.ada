-- C87B48B.ADA

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
-- POSITIONAL ACTUAL PARAMETERS CAN RESOLVE OVERLOADING OF SUBPROGRAMS.
  
-- TRH  16 AUG 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B48B IS
 
     TYPE FLAG IS (PASS, FAIL);
     TYPE INT  IS NEW INTEGER;
     TYPE BIT  IS NEW BOOLEAN;
     TYPE WHL  IS NEW INTEGER RANGE 0 .. INTEGER'LAST;
     
     GENERIC
          TYPE T1 IS PRIVATE;
          TYPE T2 IS PRIVATE;
          TYPE T3 IS PRIVATE;
          TYPE T4 IS PRIVATE;
          STAT : IN FLAG;
     PROCEDURE P1 (W : T1; X : T2; Y : T3; Z : T4);
 
     PROCEDURE P1 (W : T1; X : T2; Y : T3; Z : T4) IS
     BEGIN 
          IF STAT = FAIL THEN 
               FAILED ("RESOLUTION INCORRECT FOR OVERLOADED SUB" &
                       "PROGRAMS WITH POSITIONAL ACTUAL PARAMETERS");
          END IF;
     END P1;
  
     PROCEDURE P IS NEW P1 (WHL, INT, WHL, BIT, PASS);
     PROCEDURE P IS NEW P1 (WHL, WHL, BIT, INT, FAIL);
     PROCEDURE P IS NEW P1 (WHL, INT, BIT, WHL, FAIL);
     PROCEDURE P IS NEW P1 (INT, BIT, WHL, WHL, FAIL);
     PROCEDURE P IS NEW P1 (BIT, WHL, WHL, INT, FAIL);
     PROCEDURE P IS NEW P1 (BIT, INT, WHL, WHL, FAIL);
    
BEGIN
     TEST ("C87B48B","OVERLOADING RESOLUTION OF SUBPROGRAMS WITH" &
           " POSITIONAL ACTUAL PARAMETERS");
    
     BEGIN
          P (0, 0, 0, TRUE);
     END;
 
     RESULT;
END C87B48B;
