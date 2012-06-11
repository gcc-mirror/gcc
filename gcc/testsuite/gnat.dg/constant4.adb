-- { dg-do link }
-- { dg-options "-gnat12" }

with Constant4_Pkg; use Constant4_Pkg;

procedure Constant4 is
   Sum : Counter := 0;
begin
   for Count of Steals loop
      Sum := Sum + Count;
   end loop;
end;
