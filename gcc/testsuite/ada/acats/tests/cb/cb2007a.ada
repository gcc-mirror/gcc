-- CB2007A.ADA

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
-- CHECK THAT AN EXIT STATEMENT IN A HANDLER CAN TRANSFER CONTROL
-- OUT OF A LOOP.

-- DAT 4/13/81
--  RM 4/30/81
-- SPS 3/23/83

WITH REPORT; USE REPORT;

PROCEDURE CB2007A IS
BEGIN
     TEST ("CB2007A", "EXIT STATEMENTS IN EXCEPTION HANDLERS");

     DECLARE
          FLOW_INDEX : INTEGER := 0 ;
     BEGIN

          FOR I IN 1 .. 10 LOOP
               BEGIN
                    IF I = 1 THEN
                         RAISE CONSTRAINT_ERROR;
                    END IF;
                    FAILED ("WRONG CONTROL FLOW 1");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR => EXIT;
               END;
               FAILED ("WRONG CONTROL FLOW 2");
               EXIT;
          END LOOP;

          FOR AAA IN 1..1 LOOP
               FOR BBB IN 1..1 LOOP
                    FOR I IN 1 .. 10 LOOP
                         BEGIN
                              IF I = 1 THEN
                                   RAISE CONSTRAINT_ERROR;
                              END IF;
                              FAILED ("WRONG CONTROL FLOW A1");
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR => EXIT;
                         END;
                         FAILED ("WRONG CONTROL FLOW A2");
                         EXIT;
                    END LOOP;

                    FLOW_INDEX := FLOW_INDEX + 1 ;
               END LOOP;
          END LOOP;

          LOOP1 :
          FOR AAA IN 1..1 LOOP
               LOOP2 :
               FOR BBB IN 1..1 LOOP
                    LOOP3 :
                    FOR I IN 1 .. 10 LOOP
                         BEGIN
                              IF I = 1 THEN
                                   RAISE CONSTRAINT_ERROR;
                              END IF;
                              FAILED ("WRONG CONTROL FLOW B1");
                         EXCEPTION
                              WHEN CONSTRAINT_ERROR => EXIT LOOP2 ;
                         END;
                         FAILED ("WRONG CONTROL FLOW B2");
                         EXIT LOOP2 ;
                    END LOOP  LOOP3 ;

                    FAILED ("WRONG CONTROL FLOW B3");
               END LOOP  LOOP2 ;

               FLOW_INDEX := FLOW_INDEX + 1 ;
          END LOOP  LOOP1 ;

          IF  FLOW_INDEX /= 2  THEN  FAILED( "WRONG FLOW OF CONTROL" );
          END IF;

     END ;

     RESULT;
END CB2007A;
