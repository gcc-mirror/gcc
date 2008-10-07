-- { dg-do compile }
-- { dg-options "-g" }

procedure Addr5 (Len : Integer) is
  S : aliased String (1 .. Len) := (others => ' ');
  C : Character;
  for C'Address use S'Address;
begin
  null;
end;
