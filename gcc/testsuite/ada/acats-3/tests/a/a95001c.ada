-- A95001C.ADA

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
-- CHECK THAT IF THE BOUNDS OF THE DISCRETE RANGE OF AN ENTRY FAMILY
-- ARE INTEGER LITERALS, NAMED NUMBERS, OR ATTRIBUTES HAVING TYPE 
-- UNIVERSAL_INTEGER, BUT NOT EXPRESSIONS OF TYPE UNIVERSAL_INTEGER,
-- THE INDEX (IN AN ENTRY NAME OR ACCEPT STATEMENT) IS OF THE
-- PREDEFINED TYPE INTEGER.

-- WEI  3/4/82
-- RJK  2/1/84     ADDED TO ACVC
-- TBN  1/7/86     RENAMED FROM B950DHA-B.ADA.  ADDED NAMED CONSTANTS 
--                 AND ATTRIBUTES AS KINDS OF BOUNDS, AND MADE TEST
--                 EXECUTABLE.
-- RJW 4/11/86     RENAMED FROM C95001C-B.ADA.

WITH REPORT; USE REPORT;

PROCEDURE A95001C IS

     SUBTYPE T IS INTEGER RANGE 1 .. 10;
     I : INTEGER := 1;
     NAMED_INT1 : CONSTANT := 1;
     NAMED_INT2 : CONSTANT := 2;

     TASK T1 IS
          ENTRY E1 (1 .. 2);
          ENTRY E2 (NAMED_INT1 .. NAMED_INT2);
          ENTRY E3 (T'POS(1) .. T'POS(2));
     END T1;

     TASK BODY T1 IS
          I_INT : INTEGER := 1;
          I_POS : INTEGER := 2;
     BEGIN
          ACCEPT E1 (I_INT);
          ACCEPT E2 (I_POS);
          ACCEPT E3 (T'SUCC(1));
     END T1;

BEGIN
     TEST ("A95001C", "CHECK THAT IF THE BOUNDS OF THE DISCRETE " &
                      "RANGE OF AN ENTRY FAMILY ARE INTEGER " &
                      "LITERALS, NAMED NUMBERS, OR " &
                      "(UNIVERSAL_INTEGER) ATTRIBUTES, THE INDEX " &
                      "IS OF THE PREDEFINED TYPE INTEGER");

     T1.E1 (I);
     T1.E2 (NAMED_INT2);
     T1.E3 (T'SUCC(I));

     RESULT;
END A95001C;
