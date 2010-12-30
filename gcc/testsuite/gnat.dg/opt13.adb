-- { dg-do run }
-- { dg-options "-O" }

with Opt13_Pkg; use Opt13_Pkg;

procedure Opt13 is
  T : My_Type;
begin
  Allocate (T);
  if N /= 1 then
    raise Program_Error;
  end if;
end;
