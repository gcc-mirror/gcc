-- { dg-do run }
-- { dg-options "-O3" }
procedure Round_Div is
   type Fixed is delta 1.0 range -2147483648.0 .. 2147483647.0;
   A : Fixed := 1.0;
   B : Fixed := 3.0;
   C : Integer;
   function Divide (X, Y : Fixed) return Integer is
   begin
      return Integer (X / Y);
   end;
begin
   C := Divide (A, B);
   if C /= 0 then
      raise Program_Error;
   end if;
end Round_Div;
