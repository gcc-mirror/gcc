-- C87B24A.ADA

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
    
-- THE PREFIX OF A SLICE MUST BE APPROPRIATE FOR A ONE DIMENSIONAL
-- ARRAY TYPE.
  
-- TRH  26 JULY 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B24A IS
 
     TYPE LIST IS ARRAY (1 .. 5) OF INTEGER;
     TYPE GRID IS ARRAY (1 .. 5, 1 .. 5) OF INTEGER;
     TYPE CUBE IS ARRAY (1 .. 5, 1 .. 5, 1 .. 5) OF INTEGER;
     TYPE HYPE IS ARRAY (1 .. 5, 1 .. 5, 1 .. 5, 1 .. 5) OF INTEGER;
     TYPE FLAG IS (PASS, FAIL);
   
     L : LIST := (1 .. 5 => 0);
     G : GRID := (1 .. 5 => (1 .. 5 => 0));
     C : CUBE := (1 .. 5 => (1 .. 5 => (1 .. 5 => 0)));
     H : HYPE := (1 .. 5 => (1 .. 5 => (1 .. 5 => (1 .. 5 => 0))));
 
     GENERIC
          TYPE T IS PRIVATE;
          ARG  : IN T;
          STAT : IN FLAG;
     FUNCTION F1 RETURN T;
 
     FUNCTION F1 RETURN T IS
     BEGIN
          IF STAT = FAIL THEN 
             FAILED ("SLICE PREFIX MUST BE APPROPRIATE FOR ONE " &
                     "DIMENSIONAL ARRAY");
          END IF;
          RETURN ARG;
     END F1;
 
     FUNCTION F2 IS NEW F1 (LIST, L, PASS);
     FUNCTION F2 IS NEW F1 (GRID, G, FAIL);
     FUNCTION F2 IS NEW F1 (CUBE, C, FAIL);
     FUNCTION F2 IS NEW F1 (HYPE, H, FAIL);
    
BEGIN
     TEST ("C87B24A","OVERLOADED PREFIX FOR SLICE RESOLVED TO " &
           "ONE DIMENSIONAL ARRAY TYPE");
 
     DECLARE
          S1 : INTEGER;
 
     BEGIN
          S1 := F2 (2 .. 3)(2);
     END;
 
     RESULT;
END C87B24A;
