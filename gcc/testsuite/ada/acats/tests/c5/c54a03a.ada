-- C54A03A.ADA

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
-- CHECK THAT BOOLEAN, CHARACTER, USER-DEFINED ENUMERATED, INTEGER,
--    AND DERIVED TYPES MAY BE USED IN A CASE EXPRESSION.

-- DAT 1/22/81
-- PWB 4/22/86  RENAME TO -AB;
--              REMOVE EXTRANEOUS <CR> FROM BEGINNING OF LINE 45.

WITH REPORT;
PROCEDURE C54A03A IS

     USE REPORT;

     TYPE D_INT IS NEW INTEGER RANGE 1 .. 2;
     TYPE D_BOOL IS NEW BOOLEAN;
     TYPE D_BOOL_2 IS NEW D_BOOL;
     TYPE M_ENUM IS (FIRST, SECOND, THIRD);
     TYPE M_CHAR IS NEW CHARACTER RANGE ASCII.NUL .. 'Z';
     TYPE M_ENUM_2 IS NEW M_ENUM;

     I : INTEGER := 1;
     D_I : D_INT := 1;
     B : BOOLEAN := TRUE;
     D_B : D_BOOL := TRUE;
     D_B_2 : D_BOOL_2 := FALSE;
     E : M_ENUM := THIRD;
     C : CHARACTER := 'A';
     M_C : M_CHAR := 'Z';
     D_E : M_ENUM_2 := SECOND;

BEGIN
     TEST ("C54A03A", "CHECK VARIOUS DISCRETE TYPES " &
                      "IN CASE EXPRESSIONS");

     CASE I IS
          WHEN 2 | 3 => FAILED ("WRONG CASE 1");
          WHEN 1 => NULL;
          WHEN OTHERS => FAILED ("WRONG CASE 2");
     END CASE;

     CASE D_I IS
          WHEN 1 => NULL;
          WHEN 2 => FAILED ("WRONG CASE 2A");
     END CASE;

     CASE B IS
          WHEN TRUE => NULL;
          WHEN FALSE => FAILED ("WRONG CASE 3");
     END CASE;

     CASE D_B IS
          WHEN TRUE => NULL;
          WHEN FALSE => FAILED ("WRONG CASE 4");
     END CASE;

     CASE D_B_2 IS
          WHEN FALSE => NULL;
          WHEN TRUE => FAILED ("WRONG CASE 5");
     END CASE;

     CASE E IS
          WHEN SECOND | FIRST => FAILED ("WRONG CASE 6");
          WHEN THIRD => NULL;
     END CASE;

     CASE C IS
          WHEN 'A' .. 'Z' => NULL;
          WHEN OTHERS => FAILED ("WRONG CASE 7");
     END CASE;

     CASE M_C IS
          WHEN 'Z' => NULL;
          WHEN OTHERS => FAILED ("WRONG CASE 8");
     END CASE;

     CASE D_E IS
          WHEN FIRST => FAILED ("WRONG CASE 9");
          WHEN SECOND | THIRD => NULL;
     END CASE;

     RESULT;
END C54A03A;
