with System;
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package Unchecked_Convert9 is

   type R is record
     H : Unsigned_16;
     L : Unsigned_16;
   end record;

   Var : R;
   pragma Volatile (Var);

   function Conv is new
     Ada.Unchecked_Conversion (Source => Unsigned_32, Target => R);

   procedure Proc;

end Unchecked_Convert9;
