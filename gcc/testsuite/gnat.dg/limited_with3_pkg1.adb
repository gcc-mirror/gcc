with Ada.Strings.Fixed.Hash;

package body Limited_With3_Pkg1 is

     function Equal ( Left, Right : Element_Access) return Boolean is
     begin
        return True;
     end;

     function Equivalent_Keys (Left, Right : Key_Type) return Boolean is
     begin
        return True;
     end;

     function Hash (Key : Key_Type) return Ada.Containers.Hash_Type is
     begin
         return Ada.Strings.Fixed.Hash (Key.all);
     end Hash;

end Limited_With3_Pkg1;
