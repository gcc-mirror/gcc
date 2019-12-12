--  { dg-do compile }
--  { dg-options "-gnata" }

procedure Iter3 is
   type Mod64 is mod 2 ** 64;

   function F (X : Mod64) return Boolean is (X /= Mod64'Last);
begin
   pragma Assert (for all X in Mod64 => F(X));
   pragma Assert (for all X in Mod64'Range => F(X));

  for X in Mod64'Range loop
      null;
  end loop;
end;
