-- AC1015B.ADA

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
-- OBJECTIVE:
--     CHECK THAT WITHIN A GENERIC SUBPROGRAM THE NAME OF THE GENERIC
--     SUBPROGRAM CAN BE USED AS AN ACTUAL PARAMETER IN AN
--     INSTANTIATION.

-- HISTORY:
--     BCB 03/28/88  CREATED ORIGINAL TEST.

WITH REPORT; USE REPORT;

PROCEDURE AC1015B IS

     GENERIC
     PROCEDURE P;

     PROCEDURE P IS
          GENERIC
               WITH PROCEDURE F;
          PROCEDURE T;

          PROCEDURE T IS
          BEGIN
               NULL;
          END T;

          PROCEDURE S IS NEW T(F => P);

     BEGIN
          NULL;
     END P;

     GENERIC
     FUNCTION D RETURN BOOLEAN;

     FUNCTION D RETURN BOOLEAN IS
          GENERIC
               WITH FUNCTION L RETURN BOOLEAN;
          FUNCTION A RETURN BOOLEAN;

          FUNCTION A RETURN BOOLEAN IS
          BEGIN
               RETURN TRUE;
          END A;

          FUNCTION B IS NEW A(L => D);

     BEGIN
          RETURN TRUE;
     END D;

BEGIN
     TEST ("AC1015B", "CHECK THAT WITHIN A GENERIC SUBPROGRAM THE " &
                      "NAME OF THE GENERIC SUBPROGRAM CAN BE USED AS " &
                      "AN ACTUAL PARAMETER IN AN INSTANTIATION");

     RESULT;
END AC1015B;
