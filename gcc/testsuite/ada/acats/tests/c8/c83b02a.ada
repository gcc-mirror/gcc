-- C83B02A.ADA

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
-- CHECK THAT NESTED LOOPS CAN HAVE IDENTICALLY NAMED PARAMETERS,
--    AND REFERENCES IN THE INNERMOST LOOP ARE ASSOCIATED WITH THE
--    INNERMOST PARAMETER, ETC.


--    RM     4 JUNE 1980


WITH REPORT;
PROCEDURE  C83B02A  IS

     USE REPORT;

     I , J , K : INTEGER := 1 ;

BEGIN

     TEST ( "C83B02A" ,
            "CHECK THAT NESTED LOOPS CAN HAVE IDENTICALLY NAMED" &
            " PARAMETERS" );

                                                        --   I    J    K
     FOR  LOOP_PAR  IN  2..2  LOOP
          I := I * LOOP_PAR ;                           --   2    1    1
          FOR  LOOP_PAR  IN  3..3  LOOP
               I := I * LOOP_PAR ;                      --   6    1    1
               FOR  LOOP_PAR  IN  5..5  LOOP
                    I := I * LOOP_PAR ;                 --  30    1    1
                    FOR  SECOND_LOOP_PAR  IN  7..7  LOOP
                         J := J * SECOND_LOOP_PAR ;     --  30    7    1
                         FOR  SECOND_LOOP_PAR  IN  11..11  LOOP
                              J := J * SECOND_LOOP_PAR ;--  30   77    1
                              FOR  SECOND_LOOP_PAR  IN  13..13  LOOP
                                   J := J *
                                        SECOND_LOOP_PAR;--  30 1001    1
                              END LOOP;
                              K := K * LOOP_PAR ;       --  30 1001    5
                         END LOOP;
                         K := K * LOOP_PAR ;            --  30 1001   25
                    END LOOP;
                    K := K * LOOP_PAR ;                 --  30 1001  125
               END LOOP;
               K := K * LOOP_PAR ;                      --  30 1001  375
          END LOOP;
          K := K * LOOP_PAR ;                           --  30 1001  750
     END LOOP;

     IF  I /= 30 OR J /= 1001 OR K /= 750  THEN
           FAILED ( "DID NOT ACCESS INNERMOST ENCLOSING IDENTICALLY " &
                    "NAMED LOOP PARAMETER IN NESTED LOOPS" );
     END IF;

     RESULT;

END C83B02A;
