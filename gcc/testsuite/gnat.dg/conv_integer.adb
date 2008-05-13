-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Conv_Integer is
   S : constant := Integer'Size;
   type Regoff_T is range -1 .. 2 ** (S-1);
   for Regoff_T'Size use S;
   B : Integer;
   C : Regoff_T;
begin
   B := Integer (C);
end;
