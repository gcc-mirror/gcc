-- CA1012B4M.ADA

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
-- CHECK THAT GENERIC SUBPROGRAM DECLARATIONS AND BODIES CAN BE
--   COMPILED SEPARATELY.

-- SEPARATE FILES ARE:
--   CA1012B0  A LIBRARY GENERIC PROCEDURE DECLARATION AND BODY.
--   CA1012B2  A LIBRARY GENERIC FUNCTION DECLARATION AND BODY.
--   CA1012B4M THE MAIN PROCEDURE.

-- WKB 7/20/81

WITH REPORT, CA1012B0, CA1012B2;
USE REPORT;
PROCEDURE CA1012B4M IS

     N : INTEGER := 1;

     SUBTYPE S50 IS INTEGER RANGE 1..50;

     PROCEDURE P IS NEW CA1012B0 (S50);

     FUNCTION F IS NEW CA1012B2 (INTEGER);

BEGIN
     TEST ("CA1012B", "SEPARATELY COMPILED GENERIC SUBPROGRAM " &
                      "DECLARATIONS AND BODIES");

     P(N);
     IF N /= 2 THEN
          FAILED ("PROCEDURE NOT INVOKED");
     END IF;

     N := 1;
     IF F(N) /= 2 THEN
          FAILED ("FUNCTION NOT INVOKED");
     END IF;

     RESULT;

END CA1012B4M;
