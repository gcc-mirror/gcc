-- { dg-do compile }
-- { dg-options "-O" }

with Modular4_Pkg; use Modular4_Pkg;

procedure Modular4 is
begin
  for I in Zero .. F mod 8 loop
    raise Program_Error;
  end loop;
end;
