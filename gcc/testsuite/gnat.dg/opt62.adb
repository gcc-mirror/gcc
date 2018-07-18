-- { dg-do run }
-- { dg-options "-O" }

with Opt62_Pkg; use Opt62_Pkg;

procedure Opt62 is

   String5  : String(1..5)  := "12345";
   D: Der := (Unconstrained_Der with D2 => 5, S2 => String5);

begin
   if D.Str1 /= "abcde" then
      raise Program_Error;
   end if;
end;
