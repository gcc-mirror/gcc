-- C95022B.ADA

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
-- CHECK THAT IT IS POSSIBLE TO ACCEPT AN ENTRY CALL FROM INSIDE
-- THE BODY OF AN ACCEPT STATEMENT.

-- CHECK THE CASE OF ABORT DURING THE INNERMOST ACCEPT.

-- JEAN-PIERRE ROSEN 25-FEB-1984
-- JBG 6/1/84

WITH REPORT; USE REPORT;
PROCEDURE C95022B IS

BEGIN

     TEST("C95022B", "CHECK THAT EMBEDDED RENDEZVOUS ARE PROCESSED " &
                     "CORRECTLY (ABORT CASE)");
     DECLARE
          TASK TYPE CLIENT IS
               ENTRY GET_ID (I : INTEGER);
          END CLIENT;
     
          T_ARR : ARRAY (1..4) OF CLIENT;
     
          TASK KILL IS
               ENTRY ME;
          END KILL;
     
          TASK SERVER IS
               ENTRY E1;
               ENTRY E2;
               ENTRY E3;
               ENTRY E4;
          END SERVER;
     
          TASK BODY SERVER IS
          BEGIN
     
               ACCEPT E1 DO
                    ACCEPT E2 DO
                         ACCEPT E3 DO
                              ACCEPT E4 DO
                                   KILL.ME;
                                   E1;  -- WILL DEADLOCK UNTIL ABORT.
                              END E4;
                         END E3;
                    END E2;
               END E1;
     
          END SERVER;
     
          TASK BODY KILL IS
          BEGIN
               ACCEPT ME;
               ABORT SERVER;
          END;

          TASK BODY CLIENT IS
               ID : INTEGER;
          BEGIN
               ACCEPT GET_ID( I : INTEGER) DO
                    ID := I;
               END GET_ID;
     
               CASE ID IS
                    WHEN 1      => SERVER.E1;
                    WHEN 2      => SERVER.E2;
                    WHEN 3      => SERVER.E3;
                    WHEN 4      => SERVER.E4;
                    WHEN OTHERS => FAILED ("INCORRECT ID");
               END CASE;
     
               FAILED ("TASKING_ERROR NOT RAISED IN CLIENT" & 
                       INTEGER'IMAGE(ID));
     
          EXCEPTION
               WHEN TASKING_ERROR => 
                    NULL;
               WHEN OTHERS => 
                    FAILED("EXCEPTION IN CLIENT" & INTEGER'IMAGE(ID));
          END CLIENT;
     BEGIN
          FOR I IN 1 .. 4 LOOP
               T_ARR(I).GET_ID(I);
          END LOOP;
     END;

     RESULT;

END C95022B;
