-- C59002B.ADA

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
-- CHECK THAT JUMPS OUT OF COMPOUND STATEMENTS (OTHER THAN
--    ACCEPT STATEMENTS) ARE POSSIBLE AND ARE CORRECTLY PERFORMED.


-- FLOW OF CONTROL:   A ->  B ->  C ->  D ->   E ->  F ->  G ->  H .
--                    |     |     |     |      |     |     |
--                    IF   LOOP   CASE  BLOCK  IF   LOOP   CASE
--                                             LOOP CASE   BLOCK


--          A : GOTO B              L111 -> L311
--          FAILURE                 L121
--          E : GOTO F              L131 -> L331

--          FAILURE                 L100

--          C : GOTO D              L211 -> L411
--          FAILURE                 L221
--          G : GOTO H              L231

--          FAILURE                 L200

--          B : GOTO C              L311 -> L211
--          FAILURE                 L321
--          F : GOTO G              L331

--          FAILURE                 L300

--          D : GOTO E              L411 -> L131
--          FAILURE                 L421
--          H :                     L431 -> (OUT)

--          PRINT RESULTS


-- RM 06/05/81
-- SPS 3/8/83

WITH REPORT;
PROCEDURE  C59002B  IS

     USE  REPORT ;

BEGIN

     TEST( "C59002B" , "CHECK THAT ONE CAN JUMP OUT OF COMPOUND STATE" &
                       "MENTS" );


     DECLARE

          FLOW_STRING : STRING(1..8) := "XXXXXXXX" ;
          INDEX       : INTEGER      :=  1 ;

     BEGIN

          << L111 >>

          FLOW_STRING(INDEX) := 'A' ;
          INDEX              := INDEX + 1 ;

          IF  FALSE  THEN
               FAILED( "WRONG 'IF' BRANCH" );
          ELSE
               GOTO  L311 ;
          END IF;

          << L121 >>

          FAILED( "AT L121  -  WRONGLY" );

          << L131 >>

          FLOW_STRING(INDEX) := 'E' ;
          INDEX              := INDEX + 1 ;

          IF  FALSE  THEN
               FAILED( "WRONG 'IF' BRANCH" );
          ELSE
               FOR  J  IN  1..1  LOOP
                    GOTO  L331 ;
               END LOOP;
          END IF;

          << L100 >>

          FAILED( "AT L100  -  WRONGLY" );

          << L211 >>

          FLOW_STRING(INDEX) := 'C' ;
          INDEX              := INDEX + 1 ;

          CASE  2  IS
               WHEN  1  =>
                    FAILED( "WRONG 'CASE' BRANCH" );
               WHEN  OTHERS  =>
                    GOTO  L411 ;
          END CASE;

          << L221 >>

          FAILED( "AT L221  -  WRONGLY" );

          << L231 >>

          FLOW_STRING(INDEX) := 'G' ;
          INDEX              := INDEX + 1 ;

          CASE  2  IS
               WHEN  1  =>
                    FAILED( "WRONG 'CASE' BRANCH" );
               WHEN  OTHERS  =>
                    DECLARE
                    BEGIN
                         GOTO  L431 ;
                    END ;
          END CASE;

          << L200 >>

          FAILED( "AT L200  -  WRONGLY" );

          << L311 >>

          FLOW_STRING(INDEX) := 'B' ;
          INDEX              := INDEX + 1 ;

          FOR  I  IN  1..1  LOOP
               GOTO  L211 ;
          END LOOP;

          << L321 >>

          FAILED( "AT L321  -  WRONGLY" );

          << L331 >>

          FLOW_STRING(INDEX) := 'F' ;
          INDEX              := INDEX + 1 ;

          FOR  I  IN  1..1  LOOP
               CASE  2  IS
                    WHEN  1  =>
                         FAILED( "WRONG 'CASE' BRANCH" );
                    WHEN  OTHERS  =>
                         GOTO  L231 ;
               END CASE;
          END LOOP;

          << L300 >>

          FAILED( "AT L300  -  WRONGLY" );

          << L411 >>

          FLOW_STRING(INDEX) := 'D' ;
          INDEX              := INDEX + 1 ;

          DECLARE
               K : INTEGER := 17 ;
          BEGIN
               GOTO  L131 ;
          END;

          << L421 >>

          FAILED( "AT L421  -  WRONGLY" );

          << L431 >>

          FLOW_STRING(INDEX) := 'H' ;


          IF  FLOW_STRING /= "ABCDEFGH"  THEN
               FAILED("WRONG FLOW OF CONTROL" );
          END IF;

     END ;


     RESULT ;


END C59002B;
