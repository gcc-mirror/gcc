-- C87B40A.ADA

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
-- CHECK THAT OVERLOADING RESOLUTION USES THE FOLLOWING RULES:
--
-- THE SAME OPERATIONS ARE PREDEFINED FOR THE TYPE UNIVERSAL_INTEGER
-- AS FOR ANY INTEGER TYPE. THE SAME OPERATIONS ARE PREDEFINED FOR THE
-- TYPE UNIVERSAL_REAL AS FOR ANY FLOATING POINT TYPE. IN ADDITION
-- THESE OPERATIONS INCLUDE THE FOLLOWING MULTIPLICATION AND DIVISION
-- OPERATORS:
--
--   "*" (UNIVERSAL_REAL, UNIVERSAL_INTEGER) RETURN UNIVERSAL_REAL
--   "*" (UNIVERSAL_INTEGER, UNIVERSAL_REAL) RETURN UNIVERSAL_REAL
--   "*" (UNIVERSAL_REAL,    UNIVERSAL_REAL) RETURN UNIVERSAL_REAL
--   "*" (UNIVERSAL_INTEGER, UNIVERSAL_INTEGER) RETURN UNIVERSAL_INTEGER
--   "/" (UNIVERSAL_REAL, UNIVERSAL_INTEGER) RETURN UNIVERSAL_REAL
--  "**" (UNIVERSAL_INTEGER, INTEGER) RETURN UNIVERSAL_INTEGER
--  "**" (UNIVERSAL_REAL, INTEGER) RETURN UNIVERSAL_REAL
-- "MOD" (UNIVERSAL_INTEGER, UNIVERSAL_INTEGER) RETURN UNIVERSAL_INTEGER
-- "DIV" (UNIVERSAL_INTEGER, UNIVERSAL_INTEGER) RETURN UNIVERSAL_INTEGER
-- "ABS" (UNIVERSAL_INTEGER) RETURN UNIVERSAL INTEGER
-- "ABS" (UNIVERSAL_REAL) RETURN UNIVERSAL_REAL

-- TRH  15 SEPT 82
  
WITH REPORT; USE REPORT;
   
PROCEDURE C87B40A IS
  
     ERR : BOOLEAN := FALSE;
     B : ARRAY (1 .. 12) OF BOOLEAN := (1 .. 12 => TRUE);

     FUNCTION "-" (X : INTEGER) RETURN INTEGER
          RENAMES STANDARD."+";

     FUNCTION "+" (X : INTEGER) RETURN INTEGER IS
     BEGIN 
          ERR := TRUE;
          RETURN X;
     END "+";
 
     FUNCTION "+" (X : FLOAT) RETURN FLOAT IS
     BEGIN 
          ERR := TRUE;
          RETURN X;
     END "+";
 
BEGIN
     TEST ("C87B40A","OVERLOADING RESOLUTION OF UNIVERSAL " &
           "EXPRESSIONS");
 
     B(1)  := 1.0 * (+1) IN 0.0 .. 0.0;       -- 1.0 * 1
     B(2)  := (+1) * 1.0 IN 0.0 .. 0.0;       -- 1 * 1.0
     B(3)  := 1.0 / (+1) IN 0.0 .. 0.0;       -- 1.0 / 1
     B(4)  := (+1)  +  (+1) <= (+1)  -  (+1); -- 1+1< 1 - 1
     B(5)  := (+1)  *  (+1) >  (+1)  /  (+1); -- 1*1 > 1/1
     B(6)  := (+1) MOD (+1) /= (+1) REM (+1); -- 1 MOD 1 /= 1 REM 1

     BEGIN
          B(7)  := (+2) **  (-2) <  "-"  (-1);     -- 2**2 < 1
     EXCEPTION
          WHEN CONSTRAINT_ERROR =>
               FAILED("INCORRECT RESOLUTION FOR INTEGER EXPONENT - 7");
     END;

     B(8)  := (+1) REM (+1) > "ABS" (+1);     -- 1 REM 1 > ABS 1
     B(9)  := (+1.0)  +  (+1.0) <= (+1.0)  -  (+1.0); -- 2.0 <= 0.0
     B(10) := (+1.0)  *  (+1.0) >  (+1.0)  /  (+1.0); -- 1.0 > 1.0
     B(11) := (+2.0) **  (-1)   <  "-"  (-1.0);       -- 2.0 < 1.0
     B(12) := (+2.0) **  (-1)  <= "ABS" (+1.0);       -- 2.0 <= 1.0

     FOR I IN B'RANGE
     LOOP
          IF B(I) /= FALSE THEN
               FAILED("RESOLUTION OR OPERATIONS INCORRECT FOR "
               & "UNIVERSAL EXPRESSIONS - " & INTEGER'IMAGE(I) );
          END IF;
     END LOOP;
 
     IF ERR THEN 
          FAILED ("RESOLUTION INCORRECT FOR UNIVERSAL EXPRESSIONS");
     END IF;
 
     RESULT;
END C87B40A;
