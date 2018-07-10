-- { dg-do compile }
-- { dg-options "-O3 -gnatws" }

with SSO9_Pkg; use SSO9_Pkg;

procedure SSO9 is
   A : Arr;
begin
   Proc (A);
end;
