-- C48009A.ADA

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
-- IS RAISED IF T IS A SCALAR SUBTYPE AND X IS OUTSIDE THE RANGE OF T,
-- OR IS WITHIN T'S RANGE AND OUTSIDE OF THE RANGE OF VALUES PERMITTED
-- FOR OBJECTS DESIGNATED BY VALUES OF THE ALLOCATOR'S BASE TYPE.

-- RM  01/08/80
-- NL  10/13/81
-- SPS 10/26/82
-- JBG 03/02/83
-- EG  07/05/84
-- EDS 12/01/97  ADDED IDENT_INT TO MAKE EXPRESSION NON-STATIC.

WITH REPORT;

PROCEDURE  C48009A  IS

     USE REPORT;

BEGIN

     TEST( "C48009A" , "FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK" &
                       " THAT CONSTRAINT_ERROR IS RAISED WHEN" &
                       " APPROPRIATE - SCALAR TYPES");
     DECLARE        -- A1

          SUBTYPE  TA  IS  INTEGER RANGE 1..7;
          TYPE ATA IS ACCESS TA;
          VA : ATA;

     BEGIN

          VA  :=  NEW TA'( IDENT_INT(0) );
          FAILED ("NO EXCEPTION RAISED - 1");

     EXCEPTION

          WHEN  CONSTRAINT_ERROR  =>  NULL;
          WHEN  OTHERS      =>  FAILED ( "WRONG EXCEPTION RAISED - 1" );

     END;  -- A1

     DECLARE        -- A2

          SUBTYPE T1_7 IS INTEGER RANGE 1..7;
          TYPE AT2_6 IS ACCESS INTEGER RANGE 2..6;
          VAT2_6 : AT2_6;

     BEGIN

          BEGIN

               VAT2_6 := NEW T1_7'(1);
               FAILED ("NO EXCEPTION RAISED - 2");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS => 
                    FAILED ("WRONG EXCEPTION RAISED - 2");

          END;

          BEGIN

               VAT2_6 := NEW T1_7'(7);
               FAILED ("NO EXCEPTION RAISED - 3");

          EXCEPTION

               WHEN CONSTRAINT_ERROR => NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - 3");

          END;

     END; -- A2

     RESULT;

END C48009A;
