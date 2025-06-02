-- CA2002A2.ADA

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
-- SUBUNIT BODIES FOR STUBS GIVEN IN PACKAGE CA2002A2 IN FILE
-- CA2002A0M. 

-- BHS 8/02/84

SEPARATE (CA2002A2)
PROCEDURE PROC (Y : OUT INTEGER) IS
BEGIN
     Y := 2;
END PROC;

SEPARATE (CA2002A2)
FUNCTION FUN (Z : INTEGER := 3) RETURN BOOLEAN IS
BEGIN
     RETURN Z /= 3;
END FUN;

SEPARATE (CA2002A2)
PACKAGE BODY PKG IS
     PROCEDURE PKG_PROC (YY : IN OUT INTEGER) IS SEPARATE;
BEGIN
     I := 2;
END PKG;

SEPARATE (CA2002A2.PKG)
PROCEDURE PKG_PROC (YY : IN OUT INTEGER) IS
BEGIN
     YY := YY + 1;
END PKG_PROC;
