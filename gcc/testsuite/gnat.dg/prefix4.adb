--  { dg-do run }
--  { dg-options "-gnatX" }

with Prefix4_Pkg; use Prefix4_Pkg;

procedure Prefix4 is

  Val : T (1);

begin
  if not Val.F then
    raise Program_Error;
  end if;
end;
