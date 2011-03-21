-- { dg-do compile }
-- { dg-options "-g -O" }

with Ada.Unchecked_Conversion;

package body Unchecked_Convert8 is

   type T1 is range 0 .. 255;

   type T2 is
      record
         A : T1;
         B : T1;
      end record;

   for T2 use
      record
         A at 0 range 0 .. 7;
         B at 1 range 0 .. 7;
      end record;

   for T2'Size use 16;

   type T3 is range 0 .. (2**16 - 1);
   for  T3'Size use 16;

   function T2_TO_T3 is
      new Ada.Unchecked_Conversion (Source => T2, Target => T3);

   C : constant T3 := T2_TO_T3 (S => (A => 0, B => 0));

   procedure Dummy is begin null; end;

end Unchecked_Convert8;
