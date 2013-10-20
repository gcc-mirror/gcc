-- { dg-do compile }
-- { dg-options "-O2" }

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Opt1 is

   type Ptr is access all Integer;

   type R1 is record
      I1 : Integer;
      I2 : Integer := 0;
      I3 : Integer;
   end record;

   type R2 is record
      P  : Ptr;
      F1 : R1;
   end record;

   type R3 is record
      S  : Unbounded_String;
      F1 : R2;
      I  : Integer := 0;
      F2 : R2;
   end record;

end Opt1;
