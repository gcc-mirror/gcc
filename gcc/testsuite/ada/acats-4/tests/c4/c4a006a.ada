-- C4A006A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A UNIVERSAL_INTEGER 
-- EXPRESSION CONTAINING AN EXPONENTIATION OPERATOR IF THE EXPONENT
-- HAS A NEGATIVE VALUE.

-- BAW 9/29/80
-- SPS 4/7/82
-- TBN 10/23/85     RENAMED FROM B4A006A-B.ADA. REVISED TO CHECK FOR
--                  CONSTRAINT_ERROR WHEN EXPONENT IS NEGATIVE IN
--                  A NONSTATIC CONTEXT.

WITH REPORT; USE REPORT;
PROCEDURE C4A006A IS

BEGIN 
     TEST ("C4A006A", "CHECK THAT A NEGATIVE EXPONENT IN " &
                      "UNIVERSAL_INTEGER EXPONENTIATION RAISES " &
                      "CONSTRAINT_ERROR");

     DECLARE
          B : BOOLEAN;
     BEGIN

          B := (1 ** IDENT_INT(-1)) = 1;
          FAILED ("EXCEPTION NOT RAISED");
          IF NOT B THEN
               FAILED ("(1 ** (-1)) /= 1");
          END IF;

     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               NULL;
          WHEN OTHERS =>
               FAILED ("WRONG EXCEPTION RAISED");
     END;     

     RESULT;
END C4A006A;
