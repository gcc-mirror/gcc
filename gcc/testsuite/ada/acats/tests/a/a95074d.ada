-- A95074D.ADA

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
-- CHECK THAT 'ADDRESS, 'CONSTRAINED, 'SIZE, 'POSITION, 'FIRST_BIT,
-- AND 'LAST_BIT CAN BE APPLIED TO AN OUT PARAMETER OR OUT PARAMETER
-- SUBCOMPONENT THAT DOES NOT HAVE AN ACCESS TYPE.

-- JWC 6/25/85

WITH REPORT; USE REPORT;
WITH SYSTEM;
PROCEDURE A95074D IS
BEGIN

     TEST ("A95074D", "CHECK THAT ATTRIBUTES MAY BE APPLIED TO " &
           "NON-ACCESS FORMAL OUT PARAMETERS");

     DECLARE

          TYPE ARR IS ARRAY (1 .. 2) OF BOOLEAN;

          TYPE REC (D : INTEGER := 1) IS RECORD
               Y : BOOLEAN;
               X : ARR;
          END RECORD;

          TASK T IS
               ENTRY E (C1 : OUT ARR; C2 : OUT REC);
          END T;

          TASK BODY T IS
               X : SYSTEM.ADDRESS;
               I : INTEGER;
          BEGIN
               IF IDENT_BOOL (FALSE) THEN
                    ACCEPT E (C1 : OUT ARR; C2 : OUT REC) DO

                         C2.Y := C2'CONSTRAINED;

                         X := C1'ADDRESS;
                         X := C1(1)'ADDRESS;
                         X := C2'ADDRESS;
                         X := C2.Y'ADDRESS;

                         I := C1'SIZE;
                         I := C2.Y'SIZE;

                         I := C2.X'POSITION;
                         I := C2.Y'FIRST_BIT;
                         I := C2.Y'LAST_BIT;
                    END E;
               END IF;
          END T;

     BEGIN
          NULL;
     END;

     RESULT;

END A95074D;
