-- C4A014A.ADA

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
-- CHECK THAT ROUNDING IS DONE CORRECTLY FOR STATIC UNIVERSAL REAL
-- EXPRESSIONS.

-- JBG 5/3/85
-- JBG 11/3/85   DECLARE INTEGER CONSTANTS INSTEAD OF UNIVERSAL INTEGER
-- DTN 11/27/91  DELETED SUBPART (B).

WITH REPORT; USE REPORT;
PROCEDURE C4A014A IS

     C15  : CONSTANT := 1.5;
     C25  : CONSTANT := 2.5;
     CN15 : CONSTANT := -1.5;
     CN25 : CONSTANT := -2.5;

     C15R  : CONSTANT INTEGER := INTEGER(C15);
     C25R  : CONSTANT INTEGER := INTEGER(C25);
     CN15R : CONSTANT INTEGER := INTEGER(CN15);
     CN25R : CONSTANT INTEGER := INTEGER(CN25);

     C15_1   : BOOLEAN := 1 = C15R;
     C15_2   : BOOLEAN := 2 = C15R;
     C25_2   : BOOLEAN := 2 = C25R;
     C25_3   : BOOLEAN := 3 = C25R;

     CN15_N1 : BOOLEAN := -1 = CN15R;
     CN15_N2 : BOOLEAN := -2 = CN15R;
     CN25_N2 : BOOLEAN := -2 = CN25R;
     CN25_N3 : BOOLEAN := -3 = CN25R;

BEGIN

     TEST ("C4A014A", "CHECK ROUNDING TO INTEGER FOR UNIVERSAL REAL " &
                      "EXPRESSIONS");

     IF 1 /= INTEGER(1.4) THEN
          FAILED ("INTEGER(1.4) DOES NOT EQUAL 1");
     END IF;

     IF 2 /= INTEGER(1.6) THEN
          FAILED ("INTEGER(1.6) DOES NOT EQUAL 2");
     END IF;

     IF -1 /= INTEGER(-1.4) THEN
          FAILED ("INTEGER(-1.4) DOES NOT EQUAL -1");
     END IF;

     IF -2 /= INTEGER(-1.6) THEN
          FAILED ("INTEGER(-1.6) DOES NOT EQUAL -2");
     END IF;

     IF NOT (C15_1 OR C15_2) OR (NOT (C25_2 OR C25_3)) THEN
          FAILED ("ROUNDING OF POSITIVE VALUES NOT CORRECT");
     END IF;

     IF NOT (CN15_N1 OR CN15_N2) OR (NOT (CN25_N2 OR CN25_N3)) THEN
          FAILED ("ROUNDING OF NEGATIVE VALUES NOT CORRECT");
     END IF;

     RESULT;

END C4A014A;
