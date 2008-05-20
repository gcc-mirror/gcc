--  { dg-do compile }
with Ada.Containers.Ordered_Maps;
with Ada.Tags.Generic_Dispatching_Constructor;
package body gen_disp is
   
   use type Ada.Tags.Tag;
   
   function "<" (L, R : in Ada.Tags.Tag) return Boolean is
   begin
      return Ada.Tags.External_Tag (L) < Ada.Tags.External_Tag (R);
   end "<";
   
   package Char_To_Tag_Map is new Ada.Containers.Ordered_Maps (
      Key_Type => Character,
      Element_Type => Ada.Tags.Tag,
      "<" => "<",
      "=" => Ada.Tags. "=");
      
   package Tag_To_Char_Map is new Ada.Containers.Ordered_Maps (
      Key_Type => Ada.Tags.Tag,
      Element_Type => Character,
      "<" => "<",
      "=" => "=");
      
   use type Char_To_Tag_Map.Cursor;
   use type Tag_To_Char_Map.Cursor;
   
   Char_To_Tag : Char_To_Tag_Map.Map;
   Tag_To_Char : Tag_To_Char_Map.Map;
   
   function Get_Object is new
     Ada.Tags.Generic_Dispatching_Constructor
        (Root_Type, Ada.Streams.Root_Stream_Type'Class, Root_Type'Input);
        
   function Root_Type_Class_Input
     (S    : not null access Ada.Streams.Root_Stream_Type'Class)
      return Root_Type'Class 
   is 
      External_Tag : constant Character := Character'Input (S);
      C : constant Char_To_Tag_Map.Cursor := Char_To_Tag.Find (External_Tag);
   begin

      return Get_Object (Char_To_Tag_Map.Element (C), S);
   end Root_Type_Class_Input;
end gen_disp;
