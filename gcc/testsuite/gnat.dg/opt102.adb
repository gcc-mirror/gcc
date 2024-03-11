-- { dg-do run }
-- { dg-options "-O2 -gnata" }

with Opt102_Pkg; use Opt102_Pkg;

procedure Opt102 is
  I, F : aliased Integer;
begin
  I := Get (Two, F'Access, null);
end;
