-- C48008C.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T X", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS AN UNCONSTRAINED ARRAY TYPE WITH INDEX SUBTYPE(S) S, X
-- IS AN INDEX CONSTRAINT, AND THE BOUNDS OF X ARE NOT COMPATIBLE WITH
-- AN INDEX SUBTYPE OF T.

-- RM 01/08/80
-- NL 10/13/81
-- EG 07/05/84

WITH REPORT;

PROCEDURE C48008C IS

     USE REPORT;

BEGIN

     TEST("C48008C","FOR ALLOCATORS OF THE FORM 'NEW T X', CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                    "APPROPRIATE - UNCONSTRAINED ARRAY TYPE");

     DECLARE

          SUBTYPE  TWO  IS  INTEGER RANGE 1..2;
          TYPE  TF IS ARRAY( TWO RANGE <> ,  TWO RANGE <> ) OF INTEGER;
          TYPE  ATF  IS  ACCESS TF;
          VF  : ATF;

     BEGIN

          BEGIN
               VF  :=  NEW TF ( 0..1 , 1..2 );
               FAILED ("NO EXCEPTION RAISED - CASE 1");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>  
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 1");
          END;

          BEGIN
               VF := NEW TF(1 .. 2, 2 .. IDENT_INT(3));
               FAILED ("NO EXCEPTION RAISED - CASE 2");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - CASE 2");
          END;

     END;

     RESULT;

END C48008C;
