-- { dg-do run }
-- { dg-options "-gnatws" }

procedure alignment1 is

  type My_Integer is record
    Element : Integer;
  end record;

  F : My_Integer;

begin
  if F'Alignment /= F.Element'Alignment then
    raise Program_Error;
  end if;
end;
