-- { dg-do run }
-- { dg-options "-O2" }

with Opt106_Pkg1; use Opt106_Pkg1;

procedure Opt106 is
  Obj : T := (False, 0, 0, 0, True);

begin
  Proc (Obj, 0, False, True);
end;
