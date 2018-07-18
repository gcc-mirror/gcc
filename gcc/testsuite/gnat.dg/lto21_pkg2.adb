with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;

package body Lto21_Pkg2 is

   pragma Suppress (Tampering_Check);

   procedure Proc is

      function Hash (Syd : Natural) return Hash_Type is (Hash_Type'Mod (Syd));

      package Vect2 is new Vectors (Positive, Natural);

      package Maps is
        new Hashed_Maps (Natural, Vect2.Vector, Hash, "=", Vect2."=");

      procedure Nested (M : Maps.Map) is
         use Maps;
         procedure Inner (Position : Cursor) is null;
      begin
         Iterate (M, Inner'Access);
      end;

      M : Maps.Map;
   begin
      Nested (M);
   end;

end Lto21_Pkg2;
