-- C38108B.ADA

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
-- CHECK THAT AN INCOMPLETE TYPE CAN BE DECLARED IN THE PRIVATE PART OF
-- A LIBRARY PACKAGE, WITH THE FULL DECLARATION OCCURRING LATER IN A 
-- PACKAGE BODY.

-- AH  8/20/86

PACKAGE C38108B_P IS 
     TYPE L IS LIMITED PRIVATE;
     PROCEDURE ASSIGN (X : IN INTEGER; Y : IN OUT L);
     FUNCTION "=" (X, Y : IN L) RETURN BOOLEAN;
PRIVATE
     TYPE INC (D : INTEGER);
     TYPE L IS ACCESS INC;
END C38108B_P;

PACKAGE BODY C38108B_P IS
     TYPE INC (D : INTEGER) IS
          RECORD
               C : INTEGER;
          END RECORD;
     
     PROCEDURE ASSIGN (X : IN INTEGER; Y : IN OUT L) IS
     BEGIN
          Y := NEW INC(1);
          Y.C := X;
     END ASSIGN;

     FUNCTION "=" (X, Y : IN L) RETURN BOOLEAN IS
     BEGIN
          RETURN (X.C = Y.C);          
     END "=";

END C38108B_P;

WITH REPORT; USE REPORT;
WITH C38108B_P; USE C38108B_P;
PROCEDURE C38108B IS
     VAL_1, VAL_2 : L;
BEGIN

     TEST ("C38108B", "CHECK THAT INCOMPLETE TYPE CAN BE DECLARED IN " &
                      "PRIVATE PART WITHOUT FULL DECLARATION - " &
                      "LIBRARY PACKAGE");

     ASSIGN (2, VAL_1);
     ASSIGN (2, VAL_2);
     IF NOT "=" (VAL_1, VAL_2) THEN
          FAILED ("INCOMPLETE TYPE NOT FULLY DECLARED");
     END IF;

     RESULT;
END C38108B;
