-- CB4001A.ADA

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
-- CHECK THAT ANY EXCEPTION RAISED IN THE STATEMENT SEQUENCE OF A
-- SUBPROGRAM IS PROPAGATED TO THE CALLER OF THE SUBPROGRAM, NOT TO THE
-- STATICALLY ENCLOSING LEXICAL ENVIRONMENT.

-- RM  05/30/80
-- JRK 11/19/80
-- SPS 03/28/83
-- EG  10/30/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

WITH REPORT;
PROCEDURE CB4001A IS

     USE REPORT;

     E1 : EXCEPTION;
     I9 : INTEGER RANGE 1..10 ;
     FLOW_COUNT : INTEGER := 0 ;

BEGIN
     TEST("CB4001A","CHECK THAT ANY EXCEPTION RAISED IN THE " &
          "STATEMENT SEQUENCE OF A SUBPROGRAM IS " &
          "PROPAGATED TO THE CALLER, NOT TO THE STATICALLY ENCLOSING" &
          " LEXICAL ENVIRONMENT");

     BEGIN   --  BLOCK WITH HANDLERS; LEX. ENVIRONMT FOR ALL PROC.DEFS

          DECLARE   --  BLOCK WITH PROCEDURE DEFINITIONS

               PROCEDURE  CALLEE1 ;
               PROCEDURE  CALLEE2 ;
               PROCEDURE  CALLEE3 ;
               PROCEDURE  R ;
               PROCEDURE  S ;

               PROCEDURE  CALLER1  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1 ;
                    CALLEE1 ;
                    FAILED("EXCEPTION NOT RAISED  (CALLER1)");
               EXCEPTION
                    WHEN E1 =>
                         FLOW_COUNT := FLOW_COUNT + 1 ;
               END ;

               PROCEDURE  CALLER2  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1 ;
                    CALLEE2 ;
                    FAILED("EXCEPTION NOT RAISED  (CALLER2)");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         FLOW_COUNT := FLOW_COUNT + 1 ;
               END ;

               PROCEDURE  CALLER3  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1 ;
                    CALLEE3 ;
                    FAILED("EXCEPTION NOT RAISED  (CALLER3)");
               EXCEPTION
                    WHEN CONSTRAINT_ERROR =>
                         FLOW_COUNT := FLOW_COUNT + 1 ;
               END ;

               PROCEDURE  CALLEE1  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1 ;
                    R ;
                    FAILED("EXCEPTION NOT RAISED  (CALLEE1)");
               END ;

               PROCEDURE  CALLEE2  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1 ;
                    RAISE  CONSTRAINT_ERROR ;
                    FAILED("EXCEPTION NOT RAISED  (CALLEE2)");
               EXCEPTION
                    WHEN PROGRAM_ERROR =>
                         FAILED("WRONG EXCEPTION RAISED  (CALLEE2)");
               END ;

               PROCEDURE  CALLEE3  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 1 ;
                    I9 := IDENT_INT(20) ;
                    FAILED("EXCEPTION NOT RAISED  (CALLEE3)");
               END ;

               PROCEDURE  R  IS
                    E2 : EXCEPTION;
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 10 ;
                    S ;
                    FAILED("EXCEPTION E1 NOT RAISED (PROC R)");
               EXCEPTION
                    WHEN E2 =>
                         FAILED("WRONG EXCEPTION RAISED  (PROC R)");
               END ;

               PROCEDURE  S  IS
               BEGIN
                    FLOW_COUNT := FLOW_COUNT + 10 ;
                    RAISE  E1 ;
                    FAILED("EXCEPTION  E1  NOT RAISED  (PROC S)");
               END ;

          BEGIN   --  (THE BLOCK WITH PROC. DEFS)

               CALLER1;
               CALLER2;
               CALLER3;

          END ;   --  (THE BLOCK WITH PROC. DEFS)

     EXCEPTION

          WHEN OTHERS =>
               FAILED("EXCEPTION PROPAGATED STATICALLY");

     END ;

     IF  FLOW_COUNT /= 29  THEN
          FAILED("INCORRECT FLOW_COUNT VALUE");
     END IF;

     RESULT;
END CB4001A;
