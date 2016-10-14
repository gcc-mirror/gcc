-- { dg-do link }
-- { dg-options "-O -flto" { target lto } }

with Lto16_Pkg; use Lto16_Pkg;
with Text_IO; use Text_IO;

procedure Lto16 is
begin
  if F = 0.0 then
    Put_Line ("zero");
  else
    Put_Line ("non-zero");
  end if;
exception
  when others => Put_Line ("exception");
end;
