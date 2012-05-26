-- PR ada/46192
-- Testcase by Rolf Ebert <rolf.ebert.gcc@gmx.de>

-- { dg-do compile }
-- { dg-options "-O2 -fdump-tree-optimized" }

with System; use System;

package body Renaming5 is

   type Bits_In_Byte is array (0 .. 7) of Boolean;
   pragma Pack (Bits_In_Byte);

   A : Bits_In_Byte;
   for A'Address use System'To_Address(16#c0#);
   pragma Volatile (A);

   B : Bits_In_Byte renames A;

   procedure Proc is
   begin
      while B (0) = False loop
         null;
      end loop;
   end;

end Renaming5;

-- { dg-final { scan-tree-dump-times "goto" 3 "optimized" } }
-- { dg-final { cleanup-tree-dump "optimized" } }
