-- { dg-do compile }

with Prefix3_Pkg;

procedure Prefix3 is
begin
  Prefix3_Pkg.Handler.Log ("Hello");
end;
