-- C48009J.ADA

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
-- IS RAISED IF T IS AN UNCONSTRAINED ACCESS TYPE, ITS DESIGNATED TYPE
-- IS ALSO UNCONSTRAINED, AND A DISCRIMINANT VALUE FOR X LIES OUTSIDE
-- THE RANGE OF THE CORRESPONDING DISCRIMINANT SPECIFICATION FOR THE
-- DESIGNATED TYPE, OR A NON-NULL INDEX BOUND LIES OUTSIDE THE RANGE OF
-- AN INDEX SUBTYPE OF THE DESIGNATED TYPE.

-- EG  08/30/84

WITH REPORT;

PROCEDURE C48009J IS

     USE REPORT;

BEGIN

     TEST("C48009J","FOR ALLOCATORS OF THE FORM 'NEW T'(X)', CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN " &
                    "APPROPRIATE - ACCESS TYPE OF UNCONSTRAINED " &
                    "ACCESS TYPE");

     DECLARE

          TYPE INT IS RANGE 1 .. 5;

          TYPE UR(A : INT) IS
               RECORD
                    NULL;
               END RECORD;
          TYPE UA IS ARRAY(INT RANGE <>) OF INTEGER;

          PACKAGE P IS
               TYPE UP(A : INT) IS PRIVATE;
               TYPE UL(A : INT) IS LIMITED PRIVATE;
          PRIVATE
               TYPE UP(A : INT) IS
                    RECORD
                         NULL;
                    END RECORD;
               TYPE UL(A : INT) IS
                    RECORD
                         NULL;
                    END RECORD;
          END P;

          TYPE A_UR IS ACCESS UR;
          TYPE A_UA IS ACCESS UA;
          TYPE A_UP IS ACCESS P.UP;
          TYPE A_UL IS ACCESS P.UL;

          TYPE AA_UR IS ACCESS A_UR;
          TYPE AA_UA IS ACCESS A_UA;
          TYPE AA_UP IS ACCESS A_UP;
          TYPE AA_UL IS ACCESS A_UL;

          V_AA_UR : AA_UR;
          V_AA_UA : AA_UA;
          V_AA_UP : AA_UP;
          V_AA_UL : AA_UL;

     BEGIN

          BEGIN
               V_AA_UR := NEW A_UR'(NEW UR(INT(IDENT_INT(6))));
               FAILED ("NO EXCEPTION RAISED - UR");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UR");
          END;

          BEGIN
               V_AA_UA := NEW A_UA'(NEW UA(4 .. 7));
               FAILED ("NO EXCEPTION RAISED - UA");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UA");
          END;

          BEGIN
               V_AA_UP := NEW A_UP'(NEW P.UP(0));
               FAILED ("NO EXCEPTION RAISED - UP");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UP");
          END;

          BEGIN
               V_AA_UL := NEW A_UL'(NEW P.UL(INT(IDENT_INT(0))));
               FAILED ("NO EXCEPTION RAISED - UL");
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("WRONG EXCEPTION RAISED - UL");
          END;

     END;

     RESULT;

END C48009J;
