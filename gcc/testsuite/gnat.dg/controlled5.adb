-- { dg-do run }

with Controlled5_Pkg; use Controlled5_Pkg;

procedure Controlled5 is
   V : Root'Class := Dummy (300);
begin
   null;
end;
