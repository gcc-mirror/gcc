with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Opt50_Pkg is

   type Enum_Name is array (Enum) of access constant String;

   Enum_Name_Table : constant Enum_Name := (
      One   => new String'("one"),
      Two   => new String'("two"),
      Three => new String'("three"));

   package String_To_Enum_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type => String, Element_Type => Enum,
       Hash => Ada.Strings.Hash, Equivalent_Keys => "=");

   function Fill_Hashed_Map return String_To_Enum_Map.Map is
      Res : String_To_Enum_Map.Map;
      use String_To_Enum_Map;
   begin
      for I in Enum_Name_Table'Range loop
         declare
            Kind : constant String := Enum_Name_Table (I).all;
         begin
            Res.Insert(Key => Kind, New_Item => I);
         end;
      end loop;
      return Res;
   end;

   String_To_Enum : constant String_To_Enum_Map.Map := Fill_Hashed_Map;

   procedure Get (Kind : String; Result : out Enum; Success : out Boolean) is
      X : constant String := Ada.Characters.Handling.To_Lower (Kind);
      use String_To_Enum_Map;
      Curs : constant Cursor := String_To_Enum.Find (X);
   begin
      Success := Curs /= No_Element;
      if Success then
         Result := Element(Curs);
      end if;
   end;

   procedure Set (A : Enum_Boolean_Array) is null;

end Opt50_Pkg;
