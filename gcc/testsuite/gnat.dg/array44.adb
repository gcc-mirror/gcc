-- { dg-do compile }
-- { dg-options "-gnata" }

procedure Array44 is

   type Buffer_Type is array (Integer range -1 .. 0) of Natural range 0 .. 255;

   type U is record
      Buffer : Buffer_Type;
   end record;

   function F (Src : in Buffer_Type) return U is ((Buffer => Src));

   Src : constant Buffer_Type := (others => 1);
   Dst : constant U := F (Src);

begin
   pragma Assert (Dst.Buffer (0) = 1);
end;
