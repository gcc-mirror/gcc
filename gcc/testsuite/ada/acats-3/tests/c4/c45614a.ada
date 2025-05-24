-- C45614A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED IF THE EXPONENT VALUE IN 
-- AN INTEGER EXPONENTIATION IS NEGATIVE.
-- CHECK BOTH STATIC AND NONSTATIC EXPONENT VALUES.

-- AH  9/29/86
-- EDS 7/15/98    AVOID OPTIMIZATION

WITH REPORT; USE REPORT;
PROCEDURE C45614A IS
     INT : INTEGER :=1;
     RES : INTEGER :=0;
BEGIN
     TEST ("C45614A", "CONSTRAINT_ERROR IS RAISED FOR INTEGERS " & 
                      "HAVING A NEGATIVE EXPONENT");

     DECLARE
          E1 : CONSTANT INTEGER := -5;
     BEGIN
          RES := INT ** E1;
          FAILED ("CONSTRAINT_ERROR NOT RAISED - E1A " &
                   INTEGER'IMAGE(RES));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("CONSTRAINT_ERROR NOT RAISED - E1B");
     END;

     DECLARE
          E2 : INTEGER := 5;
     BEGIN
          RES := INT ** (-E2);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - E2A " &
                  INTEGER'IMAGE(RES));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("CONSTRAINT_ERROR NOT RAISED - E2B");
     END;

     DECLARE
          E3 : INTEGER;
     BEGIN
          E3 := IDENT_INT(-5);
          RES := INT ** E3;
          FAILED ("CONSTRAINT_ERROR NOT RAISED - E3A " &
                  INTEGER'IMAGE(RES));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("CONSTRAINT_ERROR NOT RAISED - E3B");
     END;

     DECLARE
     BEGIN
          RES := INT ** IDENT_INT(-5);
          FAILED ("CONSTRAINT_ERROR NOT RAISED - E4A " &
                  INTEGER'IMAGE(RES));

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("CONSTRAINT_ERROR NOT RAISED - E4B");
     END;

     RES := IDENT_INT(2);
     RES := IDENT_INT(RES);
     RESULT;
END C45614A;
