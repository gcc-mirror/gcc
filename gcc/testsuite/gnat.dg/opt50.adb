-- { dg-do run }
-- { dg-options "-O3 -gnatn" }

with Opt50_Pkg; use Opt50_Pkg;

procedure Opt50 is
  B : Boolean;
  E : Enum;
begin
  Get ("four", E, B);
  if B = True then
    raise Program_Error;
  end if;
  Get ("three", E, B);
  if B = False then
    raise Program_Error;
  end if;
  declare
    A : Enum_Boolean_Array (One .. E) := (others => True);
  begin
    Set (A);
  end;
end Opt50;
