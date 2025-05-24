-- C48009F.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT CONSTRAINT_ERROR
-- IS RAISED IF T IS A CONSTRAINED OR UNCONSTRAINED MULTI-DIMENSIONAL
-- ARRAY TYPE AND ALL COMPONENTS OF X DO NOT HAVE THE SAME LENGTH OR
-- BOUNDS. 

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/03/83
-- EG  07/05/84

WITH REPORT;

PROCEDURE  C48009F  IS

     USE REPORT;

BEGIN

     TEST("C48009F","FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN "          &
                    "X IS AN ILL-FORMED MULTIDIMENSIONAL AGGREGATE");

     DECLARE

          TYPE  TG00  IS  ARRAY( 4..2 ) OF  INTEGER;
          TYPE  TG10  IS  ARRAY( 1..2 ) OF  INTEGER;
          TYPE  TG20  IS  ARRAY( INTEGER RANGE <> ) OF  INTEGER;

          TYPE  TG0  IS  ARRAY( 3..2 ) OF  TG00;
          TYPE  TG1  IS  ARRAY( 1..2 ) OF  TG10;
          TYPE  TG2  IS  ARRAY( INTEGER RANGE <> ) OF  TG20(1..3);

          TYPE  ATG0  IS  ACCESS TG0;
          TYPE  ATG1  IS  ACCESS TG1;
          TYPE  ATG2  IS  ACCESS TG2;

          VG0  : ATG0;
          VG1  : ATG1;
          VG2  : ATG2;

     BEGIN

          BEGIN
               VG0  :=  NEW TG0 '( 5..4 => ( 3..1 => 2 ) );
               FAILED ("NO EXCEPTION RAISED - CASE 0");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>  NULL;
               WHEN  OTHERS            =>
                    FAILED( "WRONG EXCEPTION RAISED - CASE 0" );
          END;

          BEGIN
               VG1  :=  NEW TG1 '( ( 1 , 2 ) , ( 3 , 4 , 5 ) );
               FAILED ("NO EXCEPTION RAISED - CASE 1");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>  NULL;
               WHEN  OTHERS            =>  
                    FAILED( "WRONG EXCEPTION RAISED - CASE 1" );
          END;

          BEGIN
               VG2 := NEW TG2'( 1 => ( 1..2 => 7) , 2 => ( 1..3 => 7));
               FAILED ("NO EXCEPTION RAISED - CASE 2");
          EXCEPTION
               WHEN  CONSTRAINT_ERROR  =>  NULL;
               WHEN  OTHERS            =>  
                    FAILED( "WRONG EXCEPTION RAISED - CASE 2" );
          END;

     END;

     RESULT;

END C48009F;
