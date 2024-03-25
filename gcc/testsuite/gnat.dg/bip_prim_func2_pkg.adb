with Ada.Containers.Indefinite_Ordered_Maps;

package body BIP_Prim_Func2_Pkg is

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Integer,
      Element_Type => Some_Access);

   Map : Maps.Map;

   function Make (Key : Integer) return First'Class is
   begin
      return Map(Key).all;
   end Make;

   function Make_Delegate return First'Class is
   begin
      return Make;
   end Make_Delegate;

begin
   Map.Insert (2,Thing_Access);
end BIP_Prim_Func2_Pkg;
