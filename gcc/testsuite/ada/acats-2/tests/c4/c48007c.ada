-- C48007C.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT CONSTRAINT_ERROR IS
-- RAISED IF T IS A CONSTRAINED ARRAY TYPE AND AT LEAST ONE INDEX BOUND
-- FOR T DOES NOT EQUAL THE CORRESPONDING VALUE SPECIFIED FOR THE
-- ALLOCATOR'S BASE TYPE.

-- EG  08/10/84

WITH REPORT;

PROCEDURE C48007C IS

     USE REPORT;

BEGIN

     TEST("C48007C","FOR ALLOCATORS OF THE FORM 'NEW T' CHECK " &
                    "THAT CONSTRAINT_ERROR IS RAISED WHEN "     &
                    "APPROPRIATE - CONSTRAINED ARRAY TYPE");

     DECLARE

          TYPE UA1 IS ARRAY(INTEGER RANGE <>) OF INTEGER;
          TYPE UA2 IS ARRAY(INTEGER RANGE <>, INTEGER RANGE <>) OF
                         INTEGER;
          TYPE UA3 IS ARRAY(INTEGER RANGE <>) OF UA1(1 .. 2);

          SUBTYPE CA11 IS UA1(1 .. 3);
          SUBTYPE CA12 IS UA1(3 .. 2);
          SUBTYPE CA21 IS UA2(1 .. 2, 1 .. 2);
          SUBTYPE CA22 IS UA2(1 .. 2, 2 .. 0);
          SUBTYPE CA31 IS UA3(1 .. 2);
          SUBTYPE CA32 IS UA3(4 .. 1);

          TYPE A_UA11 IS ACCESS UA1(2 .. 4);
          TYPE A_UA12 IS ACCESS UA1(4 .. 3);
          TYPE A_UA21 IS ACCESS UA2(1 .. 3, 1 .. 2);
          TYPE A_UA22 IS ACCESS UA2(1 .. 2, 2 .. 1);
          TYPE A_UA31 IS ACCESS UA3(1 .. 3);
          TYPE A_UA32 IS ACCESS UA3(3 .. 1);

          V11 : A_UA11;
          V12 : A_UA12;
          V21 : A_UA21;
          V22 : A_UA22;
          V31 : A_UA31;
          V32 : A_UA32;

     BEGIN

          BEGIN -- V11

               V11 := NEW CA11;
               FAILED("NO EXCEPTION RAISED - V11");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - V11");

          END;

          BEGIN -- V12

               V12 := NEW CA12;
               FAILED("NO EXCEPTION RAISED - V12");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - V12");

          END;

          BEGIN -- V21

               V21 := NEW CA21;
               FAILED("NO EXCEPTION RAISED - V21");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - V21");

          END;

          BEGIN -- V22

               V22 := NEW CA22;
               FAILED("NO EXCEPTION RAISED - V22");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - V22");

          END;

          BEGIN -- V31

               V31 := NEW CA31;
               FAILED("NO EXCEPTION RAISED - V31");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - V31");

          END;

          BEGIN -- V32

               V32 := NEW CA32;
               FAILED("NO EXCEPTION RAISED - V32");

          EXCEPTION

               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED("WRONG EXCEPTION RAISED - V32");

          END;

     END;

     RESULT;

END C48007C;
