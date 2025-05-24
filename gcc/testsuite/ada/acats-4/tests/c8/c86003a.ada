-- C86003A.ADA

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
-- CHECK THAT  'STANDARD'  IS NOT TREATED AS A RESERVED WORD IN
--    SELECTED COMPONENT NAMES.

-- RM  01/21/80
-- EG  10/29/85  ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.
-- RLB 06/29/01  CORRECTED TO ALLOW AGGRESSIVE OPTIMIZATION.

WITH REPORT ;
PROCEDURE  C86003A  IS

     USE REPORT ;

BEGIN

     TEST("C86003A" , "CHECK THAT  'STANDARD'  IS NOT TREATED AS A" &
                      " RESERVED WORD IN SELECTED COMPONENT NAMES" );

     DECLARE    -- A
     BEGIN

          DECLARE

               PACKAGE  STANDARD  IS
                    CHARACTER      :  BOOLEAN ;
                    TYPE  INTEGER  IS (FALSE, TRUE) ;
                    CONSTRAINT_ERROR :  EXCEPTION ;
               END  STANDARD ;

               TYPE  REC2  IS
                    RECORD
                         AA , BB  :  BOOLEAN  := FALSE ;
                    END RECORD;

               TYPE  REC1  IS
                    RECORD
                         STANDARD : REC2 ;
                    END RECORD;

               A    :  REC1 ;
               TYPE  ASI  IS  ACCESS STANDARD.INTEGER ;
               VASI  :  ASI ;
               VI : INTEGER RANGE 1 .. 10;   -- THE "REAL" STANDARD
                                             -- TYPE 'INTEGER'

          BEGIN

               VASI  :=  NEW STANDARD.INTEGER'(STANDARD.FALSE);
               STANDARD.CHARACTER  :=  A.STANDARD.BB ;

               IF  STANDARD.CHARACTER  THEN FAILED( "RES. (VAR.)" );
               END IF;

               VI  :=  IDENT_INT(11);   -- TO CAUSE THE "REAL"
                                        -- (PREDEFINED) CONSTRAINT_ERROR
                                        -- EXCEPTION.
               IF VI /= IDENT_INT(11) THEN
                   FAILED ("WRONG VALUE - V1");
               ELSE
                   FAILED ("OUT OF RANGE VALUE - V1");
               END IF;
          EXCEPTION

               WHEN STANDARD.CONSTRAINT_ERROR => FAILED ("RES. (EXC.)");

               WHEN CONSTRAINT_ERROR => NULL;

               WHEN OTHERS => FAILED ("WRONG EXCEPTION RAISED - A");

          END ;

     EXCEPTION

          WHEN  OTHERS  =>  FAILED( "EXCEPTION RAISED BY DECL. (A)" );

     END ;    -- A


     DECLARE    -- B

          TYPE  REC  IS
               RECORD
                    INTEGER  :  BOOLEAN  := FALSE ;
               END RECORD;

          STANDARD :  REC ;

     BEGIN

          IF  STANDARD.INTEGER  THEN  FAILED( "RESERVED  -  REC.,INT.");
          END IF;

     END ;    -- B


     RESULT ;


END C86003A ;
