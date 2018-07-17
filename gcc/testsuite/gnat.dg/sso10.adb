--  { dg-do run }

with SSO10_Pkg; use SSO10_Pkg;

procedure SSO10 is

  procedure Inner (R : Root'Class) is
  begin
    Run (R);
  end;

  R : Root;

begin
  Inner (R);
end;
