--  { dg-do compile }

with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;

procedure Renaming15 is
   use Ada.Containers;

   subtype String_T is String (1 .. 3);

   function Hash (Aircraft_Id : Integer) return Hash_Type is
       (Hash_Type (Aircraft_Id) * (2 ** 31 - 1));
   function Equal (Left, Right : Integer) return Boolean is (Left = Right);
   package Radar_Map is new Hashed_Maps (Integer, String_T, Hash, Equal);

   Radars : Radar_Map.Map;

   procedure Change_Elem_Value is
   begin
      for C in Radars.Iterate loop
         declare
            E : String_T renames Radar_Map.Element (C);
         begin
            E := "Xyz";  --  { dg-error "left hand side of assignment must be a variable" }
            Ada.Text_IO.Put_Line (E);
         end;
      end loop;
   end Change_Elem_Value;
begin
   Radars.Include (1, "jjj");
   Change_Elem_Value;
end Renaming15;
