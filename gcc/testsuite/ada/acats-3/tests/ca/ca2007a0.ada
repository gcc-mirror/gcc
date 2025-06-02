-- CA2007A0M.ADA

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
-- CHECK THAT SUBUNIT PACKAGES ARE ELABORATED IN THE ORDER IN
--   WHICH THEIR BODY STUBS APPEAR, NOT (NECESSARILY) IN THE
--   ORDER IN WHICH THEY ARE COMPILED.

-- SEPARATE FILES ARE:
--   CA2007A0M THE MAIN PROCEDURE.
--   CA2007A1  A SUBUNIT PACKAGE BODY.
--   CA2007A2  A SUBUNIT PACKAGE BODY.
--   CA2007A3  A SUBUNIT PACKAGE BODY.

-- WKB 7/1/81
-- JRK 7/1/81

WITH REPORT;
USE REPORT;
PROCEDURE CA2007A0M IS

     ELAB_ORDER : STRING (1..3) := "   ";
     NEXT : NATURAL := 1;

     PACKAGE CALL_TEST IS 
     END CALL_TEST;

     PACKAGE BODY CALL_TEST IS
     BEGIN
          TEST ("CA2007A", "CHECK THAT SUBUNIT PACKAGES ARE " &
                           "ELABORATED IN THE ORDER IN WHICH THEIR " &
                           "BODY STUBS APPEAR");
     END CALL_TEST;

     PACKAGE CA2007A3 IS
     END CA2007A3;

     PACKAGE BODY CA2007A3 IS SEPARATE;

     PACKAGE CA2007A2 IS
     END CA2007A2;

     PACKAGE BODY CA2007A2 IS SEPARATE;

     PACKAGE CA2007A1 IS
     END CA2007A1;

     PACKAGE BODY CA2007A1 IS SEPARATE;

BEGIN

     IF ELAB_ORDER /= "321" THEN
          FAILED ("INCORRECT ELABORATION ORDER");
     END IF;

     RESULT;
END CA2007A0M;
