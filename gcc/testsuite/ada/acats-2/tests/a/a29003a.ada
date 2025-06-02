-- A29003A.ADA

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
-- CHECK THAT ALL PREDEFINED ATTRIBUTES EXCEPT DIGITS, DELTA, AND RANGE,
-- AND ALL PREDEFINED TYPE AND PACKAGE NAMES ARE NOT RESERVED WORDS.

-- AH  8/11/86

WITH REPORT; USE REPORT;
PROCEDURE A29003A IS
     SUBTYPE INT IS INTEGER;

-- PREDEFINED ATTRIBUTES

     ADDRESS           : INT := IDENT_INT(0);        -- ATTRIBUTE
     AFT               : INT := IDENT_INT(0);        -- ATTRIBUTE
     BASE              : INT := IDENT_INT(0);        -- ATTRIBUTE
     CALLABLE          : INT := IDENT_INT(0);        -- ATTRIBUTE
     CONSTRAINED       : INT := IDENT_INT(0);        -- ATTRIBUTE
     COUNT             : INT := IDENT_INT(0);        -- ATTRIBUTE
     EMAX              : INT := IDENT_INT(0);        -- ATTRIBUTE
     EPSILON           : INT := IDENT_INT(0);        -- ATTRIBUTE
     FIRST             : INT := IDENT_INT(0);        -- ATTRIBUTE
     FIRST_BIT         : INT := IDENT_INT(0);        -- ATTRIBUTE
     FORE              : INT := IDENT_INT(0);        -- ATTRIBUTE
     IMAGE             : INT := IDENT_INT(0);        -- ATTRIBUTE
     LARGE             : INT := IDENT_INT(0);        -- ATTRIBUTE
     LAST              : INT := IDENT_INT(0);        -- ATTRIBUTE
     LAST_BIT          : INT := IDENT_INT(0);        -- ATTRIBUTE
     LENGTH            : INT := IDENT_INT(0);        -- ATTRIBUTE
     MACHINE_EMAX      : INT := IDENT_INT(0);        -- ATTRIBUTE
     MACHINE_EMIN      : INT := IDENT_INT(0);        -- ATTRIBUTE
     MACHINE_MANTISSA  : INT := IDENT_INT(0);        -- ATTRIBUTE
     MACHINE_OVERFLOWS : INT := IDENT_INT(0);        -- ATTRIBUTE
     MACHINE_RADIX     : INT := IDENT_INT(0);        -- ATTRIBUTE
     MACHINE_ROUNDS    : INT := IDENT_INT(0);        -- ATTRIBUTE
     MANTISSA          : INT := IDENT_INT(0);        -- ATTRIBUTE
     POS               : INT := IDENT_INT(0);        -- ATTRIBUTE
     POSITION          : INT := IDENT_INT(0);        -- ATTRIBUTE
     PRED              : INT := IDENT_INT(0);        -- ATTRIBUTE
     SAFE_EMAX         : INT := IDENT_INT(0);        -- ATTRIBUTE
     SAFE_LARGE        : INT := IDENT_INT(0);        -- ATTRIBUTE
     SAFE_SMALL        : INT := IDENT_INT(0);        -- ATTRIBUTE
     SIZE              : INT := IDENT_INT(0);        -- ATTRIBUTE
     SMALL             : INT := IDENT_INT(0);        -- ATTRIBUTE
     STORAGE_SIZE      : INT := IDENT_INT(0);        -- ATTRIBUTE
     SUCC              : INT := IDENT_INT(0);        -- ATTRIBUTE
     TERMINATED        : INT := IDENT_INT(0);        -- ATTRIBUTE
     VAL               : INT := IDENT_INT(0);        -- ATTRIBUTE
     VALUE             : INT := IDENT_INT(0);        -- ATTRIBUTE
     WIDTH             : INT := IDENT_INT(0);        -- ATTRIBUTE

-- PREDEFINED TYPES

     BOOLEAN   : INT := IDENT_INT(0);                -- TYPE
     CHARACTER : INT := IDENT_INT(0);                -- TYPE
     DURATION  : INT := IDENT_INT(0);                -- TYPE
     FLOAT     : INT := IDENT_INT(0);                -- TYPE
     INTEGER   : INT := IDENT_INT(0);                -- TYPE
     NATURAL   : INT := IDENT_INT(0);                -- TYPE
     POSITIVE  : INT := IDENT_INT(0);                -- TYPE
     STRING    : INT := IDENT_INT(0);                -- TYPE

-- PREDEFINED PACKAGE NAMES

     ASCII                  : INT := IDENT_INT(0);     -- PACKAGE
     CALENDAR               : INT := IDENT_INT(0);     -- PACKAGE
     DIRECT_IO              : INT := IDENT_INT(0);     -- PACKAGE
     IO_EXCEPTIONS          : INT := IDENT_INT(0);     -- PACKAGE
     LOW_LEVEL_IO           : INT := IDENT_INT(0);     -- PACKAGE
     MACHINE_CODE           : INT := IDENT_INT(0);     -- PACKAGE
     SEQUENTIAL_IO          : INT := IDENT_INT(0);     -- PACKAGE
     SYSTEM                 : INT := IDENT_INT(0);     -- PACKAGE
     TEXT_IO                : INT := IDENT_INT(0);     -- PACKAGE
     UNCHECKED_CONVERSION   : INT := IDENT_INT(0);     -- PACKAGE
     UNCHECKED_DEALLOCATION : INT := IDENT_INT(0);     -- PACKAGE

BEGIN
     TEST("A29003A", "NO ADDITIONAL RESERVED WORDS");
     RESULT;
END A29003A;
