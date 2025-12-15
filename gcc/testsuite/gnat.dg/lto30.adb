-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

with Ada.Unchecked_Conversion;
with System;

package body Lto30 is

   generic
      type T is private;
   package Unbounded_Arrays is
      type Unbounded_Array is array (Natural range 1 .. Natural'Last) of T;
      type Unbounded_Array_Access is access Unbounded_Array;
      function Convert is new
         Ada.Unchecked_Conversion (System.Address, Unbounded_Array_Access);
   end Unbounded_Arrays;

   package Atom_Arrays is new Unbounded_Arrays (Ptr);
   use Atom_Arrays;

   procedure Proc is
      procedure Foo (Targets : access Unbounded_Array_Access);
      pragma Import (Ada, Foo, "Foo");

      Output : aliased Unbounded_Array_Access;

   begin
      Foo (Output'Unchecked_Access);
   end;

end Lto30;
