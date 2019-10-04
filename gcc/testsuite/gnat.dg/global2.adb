--  { dg-do compile }

package body Global2 is
   procedure Change_X is
   begin
      X.all := 1;
   end Change_X;
   procedure Change2_X is
   begin
      X.all := 1;
   end Change2_X;
end Global2;