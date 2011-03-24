-- { dg-do compile }
-- { dg-options "-O -gnatws" }

package body Array15 is

   type Arr is array (Natural range <>) of Integer;

   Table : Arr (1 .. 4);

   N : Natural := 1;

   procedure Zero is
   begin
      N := 0;
   end;

   function F (I : Integer) return Integer is
      A1 : Arr := (1 => I);
      A2 : Arr := Table (1 .. N) & A1;
   begin
      return A2 (I);
   end;

end Array15;
