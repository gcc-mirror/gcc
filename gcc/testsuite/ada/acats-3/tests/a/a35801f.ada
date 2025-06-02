-- A35801F.ADA

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
-- CHECK THAT THE ATTRIBUTES FIRST AND LAST RETURN VALUES HAVING THE
-- SAME BASE TYPE AS THE PREFIX WHEN THE PREFIX IS A FLOATING POINT
-- TYPE.

-- THIS CHECK IS PROVIDED THROUGH THE USE OF THIS TEST IN CONJUNCTION
-- WITH TEST B35801C.

-- R.WILLIAMS 8/21/86

WITH REPORT; USE REPORT;
PROCEDURE A35801F IS

     TYPE REAL IS DIGITS 3 RANGE -100.0 .. 100.0;
     SUBTYPE SURREAL IS REAL RANGE -50.0 .. 50.0;

     TYPE NFLT IS NEW FLOAT;
     SUBTYPE UNIT IS NFLT RANGE -1.0 .. 1.0;     

     SUBTYPE EMPTY IS FLOAT RANGE 1.0 .. -1.0;

     R1 : REAL := SURREAL'FIRST;       -- OK.
     R2 : REAL := SURREAL'LAST;        -- OK.

     N1 : NFLT := UNIT'FIRST;          -- OK.
     N2 : NFLT := UNIT'LAST;           -- OK.

     F1 : FLOAT := FLOAT'FIRST;        -- OK.
     F2 : FLOAT := FLOAT'LAST;         -- OK.

     E1 : FLOAT := EMPTY'FIRST;        -- OK.
     E2 : FLOAT := EMPTY'LAST;         -- OK.

BEGIN
     TEST ( "A35801F", "CHECK THAT THE ATTRIBUTES FIRST AND LAST " &
                       "RETURN VALUES HAVING THE SAME BASE TYPE AS " &
                       "THE PREFIX WHEN THE PREFIX IS A FLOATING " &
                       "POINT TYPE" );

     RESULT;
END A35801F;
