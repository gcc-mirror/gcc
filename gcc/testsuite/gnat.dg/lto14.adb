-- { dg-do link }
-- { dg-options "-largs -f -margs -flto" { target lto } }
-- { dg-skip-if "missing linker support" { *-*-solaris2.* } }

procedure Lto14 is
begin
  null;
end;
