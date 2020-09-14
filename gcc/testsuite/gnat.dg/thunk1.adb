-- { dg-do run }

with Thunk1_Pkg1; use Thunk1_Pkg1;

procedure Thunk1 is
  D: Derived;
begin
  D.Op ("Message");
end;
