--  { dg-do compile }

package body Array32 is

   procedure Init (A : out Arr) is
   begin
      A := ((I => 1), (I => 2));
   end;

end Array32;
