-- { dg-do run }
-- { dg-options "-O" }

with Interfaces; use Interfaces;
with SSO8_Pkg; use SSO8_Pkg;

procedure SSO8 is
  Data : Rec;
begin
  Data.Array_Data (2) := True;
  Val8 := Conv (Data);
  if Val8 /= 32 then
    raise Program_Error;
  end if;
end;
