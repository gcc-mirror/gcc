generic
   type First(<>) is abstract tagged limited private;
   type Second(<>) is new First with private;
   with function Make return Second is <>;
package BIP_Prim_Func2_Pkg is

   function Make (Key : Integer) return First'Class;

private

   type Some_Access is not null access function return First'Class;

   function Make_Delegate return First'Class;

   Thing_Access : constant Some_Access := Make_Delegate'Access;

end BIP_Prim_Func2_Pkg;
