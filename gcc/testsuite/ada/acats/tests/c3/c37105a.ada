-- C37105A.ADA

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
-- CHECK THAT RECORDS WITH ONLY DISCRIMINANTS ARE OK.

-- DAT 5/18/81
-- JWC 6/28/85   RENAMED TO -AB

WITH REPORT; USE REPORT;

PROCEDURE C37105A IS
BEGIN
     TEST ("C37105A", "RECORDS WITH ONLY DISCRIMINANTS");

     DECLARE
          TYPE R1 (D : BOOLEAN) IS RECORD
               NULL; END RECORD;
          TYPE R2 (D, E : BOOLEAN) IS RECORD
               NULL; END RECORD;
          TYPE R3 (A,B,C,D : INTEGER; W,X,Y,Z : CHARACTER) IS
               RECORD NULL; END RECORD;
          OBJ1 : R1 (IDENT_BOOL(TRUE));
          OBJ2 : R2 (IDENT_BOOL(FALSE), IDENT_BOOL(TRUE));
          OBJ3 : R3 (1,2,3,4,'A','B','C',IDENT_CHAR('D'));
     BEGIN
          IF OBJ1 = (D => (FALSE))
          OR OBJ2 /= (FALSE, (TRUE))
          OR OBJ3 /= (1,2,3,4,'A','B','C',('D'))
          THEN FAILED ("DISCRIMINANT-ONLY RECORDS DON'T WORK");
          END IF;
     END;

     RESULT;
END C37105A;
