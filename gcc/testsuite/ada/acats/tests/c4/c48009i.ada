-- C48009I.ADA

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
-- IS RAISED IF THE DESIGNATED TYPE FOR "NEW T'(X)" IS A CONSTRAINED
-- ACCESS TYPE, CA, T IS CA'BASE, AND A DISCRIMINANT OR INDEX VALUE OF X
-- DOES NOT EQUAL A VALUE SPECIFIED FOR CA.

-- EG  08/30/84

WITH REPORT;

PROCEDURE C48009I IS

     USE REPORT;

BEGIN

     TEST("C48009I","FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                    "APPROPRIATE - ACCESS TYPE OF CONSTRAINED " &
                    "ACCESS TYPE");

     DECLARE

          TYPE UR(A : INTEGER) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE UA IS ARRAY(INTEGER RANGE <>) OF INTEGER;
          
          PACKAGE P IS
               TYPE UP(A : INTEGER) IS PRIVATE;
               TYPE UL(A : INTEGER) IS LIMITED PRIVATE;
          PRIVATE
               TYPE UP(A : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
               TYPE UL(A : INTEGER) IS
                    RECORD
                         NULL;
                    END RECORD;
          END P;

          TYPE A_UR IS ACCESS UR;
          TYPE A_UA IS ACCESS UA;
          TYPE A_UP IS ACCESS P.UP;
          TYPE A_UL IS ACCESS P.UL;

          TYPE AC_A_UR IS ACCESS A_UR(2);
          TYPE AC_A_UA IS ACCESS A_UA(2 .. 4);
          TYPE AC_A_UP IS ACCESS A_UP(3);
          TYPE AC_A_UL IS ACCESS A_UL(4);

          V_AC_A_UR : AC_A_UR;
          V_AC_A_UA : AC_A_UA;
          V_AC_A_UP : AC_A_UP;
          V_AC_A_UL : AC_A_UL;

     BEGIN

          BEGIN
               V_AC_A_UR := NEW A_UR'(NEW UR(3));
               FAILED ("NO EXCEPTION RAISED - UR");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UR");
          END;

          BEGIN
               V_AC_A_UA := NEW A_UA'(NEW UA(3 .. 5));
               FAILED ("NO EXCEPTION RAISED - UA");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UA");
          END;

          BEGIN
               V_AC_A_UP := NEW A_UP'(NEW P.UP(IDENT_INT(4)));
               FAILED ("NO EXCEPTION RAISED - UP");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UP");
          END;

          BEGIN
               V_AC_A_UL := NEW A_UL'(NEW P.UL(IDENT_INT(5)));
               FAILED ("NO EXCEPTION RAISED - UL");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UL");
          END;

     END;

     RESULT;

END C48009I;
