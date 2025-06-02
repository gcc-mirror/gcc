-- C83B02B.ADA

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
-- CHECK THAT NON-NESTED LOOPS CAN HAVE IDENTICALLY NAMED PARAMETERS,
--    AND REFERENCES IN EACH LOOP ARE ASSOCIATED WITH THAT LOOP'S
--    LOOP PARAMETER.  (THIS IS PART  B  OF THE OBJECTIVE.)
-- CHECK ALSO THAT A LOOP PARAMETER CAN HAVE THE SAME IDENTIFIER
--    AS A VARIABLE DECLARED IN THE SCOPE IMMEDIATELY CONTAINING
--    THE LOOP.  (THIS IS PART  C  OF THE OBJECTIVE.)



--    RM     6 JUNE 1980


WITH REPORT;
PROCEDURE  C83B02B  IS

     USE REPORT;

     I , J : INTEGER := 1 ;

BEGIN

     TEST ( "C83B02B" ,
            "CHECK THAT NON-NESTED LOOPS CAN HAVE IDENTICALLY NAMED" &
            " PARAMETERS" );

     COMMENT ( "THE NAME MAY BE THE SAME AS THAT OF A VARIABLE" &
               " KNOWN OUTSIDE THE LOOP" );

     -- CHECK PART B OF THE OBJECTIVE
     DECLARE
          TYPE  WEEKDAY  IS  ( MON , TUE , WED , THU , FRI );
     BEGIN

          FOR  LOOP_PAR  IN  3..3  LOOP
               I := I * LOOP_PAR ;              --    3
          END LOOP;

          FOR  LOOP_PAR  IN  FRI..FRI  LOOP
               I := I * WEEKDAY'POS(LOOP_PAR) ; --   12
          END LOOP;

          FOR  LOOP_PAR  IN  7..7  LOOP
               I := I * LOOP_PAR ;              --  84
          END LOOP;

     END;

     IF I /= 84 THEN
          FAILED ("DID NOT ACCESS ENCLOSING IDENTICALLY NAMED " &
                  "LOOP PARAMETER IN NON-NESTED LOOPS");
     END IF;

     -- CHECK PART C OF THE OBJECTIVE
     DECLARE
          LOOP_PAR : INTEGER := 2 ;
     BEGIN

          J := J * LOOP_PAR ;                          --    2

          FOR  LOOP_PAR  IN  3..3  LOOP
               J := J * LOOP_PAR ;                     --    6
          END LOOP;

          J := J * LOOP_PAR ;                          --   12

          FOR  LOOP_PAR  IN  5..5  LOOP
               J := J * LOOP_PAR ;                     --   60
          END LOOP;

          J := J * LOOP_PAR ;                          --  120

          FOR  LOOP_PAR  IN  7..7  LOOP
               J := J * LOOP_PAR ;                     --  840
          END LOOP;

          J := J * LOOP_PAR ;                          -- 1680

     END;

     IF J /= 1680 THEN
          FAILED ("DID NOT ACCESS IDENTICALLY NAMED LOOP PARAMETER " &
                  "INSIDE NON-NESTED LOOPS OR IDENTICALLY NAMED " &
                  "VARIABLE OUTSIDE LOOPS");
     END IF;

     RESULT;

END C83B02B;
