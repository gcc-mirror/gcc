-- { dg-do compile }
-- { dg-options "-O" }

procedure Opt66 (I : Integer) is
  E : exception;
begin
  if I = 0 then
    raise E;
  end if;
  Opt66 (I - I / abs (I));
exception
  when others => null;
end;
