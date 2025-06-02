-- C42006A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED WHEN A STRING LITERAL OF AN
-- ARRAY TYPE CONTAINS A CHARACTER THAT DOES NOT BELONG TO THE COMPONENT
-- SUBTYPE.

-- SPS 2/22/84
-- EDS 12/02/97  MODIFIED THE COMPONENT SUBTYPES SO THAT THEY ARE NON-STATIC.
-- EDS 7/14/98    AVOID OPTIMIZATION

WITH REPORT;
USE REPORT;
PROCEDURE C42006A IS
BEGIN

     TEST ("C42006A", "CHECK THAT THE VALUES OF STRING LITERALS MUST" &
           " BELONG TO THE COMPONENT SUBTYPE.");

     DECLARE

          TYPE CHAR_COMP IS ('A', 'B', 'C', 'D', 'E', 'F');

          ASCIINUL : CHARACTER := ASCII.NUL;
          SUBTYPE NON_GRAPHIC_CHAR IS CHARACTER 
               RANGE ASCIINUL .. ASCII.BEL;

          BEE : CHAR_COMP := 'B';
          TYPE CHAR_STRING IS ARRAY (POSITIVE RANGE <>) 
               OF CHAR_COMP RANGE BEE..'C';
          TYPE NON_GRAPHIC_CHAR_STRING IS ARRAY (POSITIVE RANGE <>)
               OF NON_GRAPHIC_CHAR;

          C_STR : CHAR_STRING (1 .. 1);
          C_STR_5 : CHAR_STRING (1 .. 5) := "BBBBB";
          N_G_STR : NON_GRAPHIC_CHAR_STRING (1 .. 1) := 
                    (OTHERS => NON_GRAPHIC_CHAR'FIRST);

     BEGIN

          BEGIN
               C_STR_5 := "BABCC";      -- 'A' NOT IN COMPONENT SUBTYPE.
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 1 " & 
                       CHAR_COMP'IMAGE(C_STR_5(1)));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("SOME EXCEPTION RAISED - 1");
          END;

          BEGIN
               C_STR_5 := "BCBCD";      -- 'D' NOT IN COMPONENT SUBTYPE.
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 2 " & 
                       CHAR_COMP'IMAGE(C_STR_5(1)));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("SOME EXCEPTION RAISED - 2");
          END;

          BEGIN
               N_G_STR := "Z";
               FAILED ("CONSTRAINT_ERROR NOT RAISED - 3 " & 
                       INTEGER'IMAGE(CHARACTER'POS(N_G_STR(1))));
          EXCEPTION
               WHEN CONSTRAINT_ERROR =>
                    NULL;
               WHEN OTHERS =>
                    FAILED ("SOME EXCEPTION RAISED - 3");
          END;

     END;

     RESULT;

END C42006A;
