-- { dg-do run }
-- { dg-options "-O2" }

with Interfaces;
with Opt61_Pkg; use Opt61_Pkg;

procedure Opt61 is

   use type Interfaces.Integer_64;

   X : constant Int64 := 3125;
   Y : constant Int64 := 5;
   Z : constant Int64 := 10;
   Q, R: Int64;

begin
   Double_Divide (X, Y, Z, Q, R, False);
   if R /= 25 then
     raise Program_Error;
   end if;
end;
