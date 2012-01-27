with Ada.Containers.Hashed_Maps;

generic

     type Object_Type is tagged private;

package Limited_With3_Pkg1 is

     type Key_Type is access all String;

     type Element_Type is new Object_Type with null record;

     type Element_Access is access all Element_Type;

     function Equal (Left, Right : Element_Access) return Boolean;

     function Equivalent_Keys (Left, Right : Key_Type) return Boolean;

     function Hash (Key : Key_Type) return Ada.Containers.Hash_Type;

     package Table_Package is new Ada.Containers.Hashed_Maps (
         Key_Type            => Key_Type,
         Element_Type        => Element_Access,
         Hash                => Hash,
         Equivalent_Keys     => Equivalent_Keys,
         "="                 => Equal);

end Limited_With3_Pkg1;
