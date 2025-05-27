--  { dg-do run }
--  { dg-options "-O2" }

with SSO19_Pkg; use SSO19_Pkg;

procedure SSO19 is
  R : constant Rec := (D => (I => 8, F => 4.6095713E-41));

begin
  if not Is_Valid (R) then
    raise Program_Error;
  end if;
end;
