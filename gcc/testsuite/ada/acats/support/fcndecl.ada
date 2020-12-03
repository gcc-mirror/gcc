-- FCNDECL.ADA
--
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
--
-- PACKAGE THAT MAY BE MODIFIED TO DECLARE FUNCTIONS THAT RETURN
-- VALUES USABLE FOR INITIALIZATION OF CONSTANTS IN PACKAGE SPPRT13.

WITH SYSTEM;
PACKAGE FCNDECL IS
-- INSERT FUNCTION DECLARATIONS AS NEEDED.

  type Mem is array (1 .. 100) of Long_Long_Integer;
  Var0: Mem;
  Var1: Mem;
  Var2: Mem;

  Var_Addr : constant System.Address := Var0'address;
  Var_Addr1: constant System.Address := Var1'address;
  Var_Addr2: constant System.Address := Var2'address;

  Ent0: Mem;
  Ent1: Mem;
  Ent2: Mem;

  Entry_Addr : constant System.Address := Ent0'address;
  Entry_Addr1: constant System.Address := Ent0'address;
  Entry_Addr2: constant System.Address := Ent0'address;

END FCNDECL;
