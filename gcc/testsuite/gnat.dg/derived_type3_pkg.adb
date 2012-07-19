with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Derived_Type3_Pkg is

   type Parent is tagged null record;

   type Child is new Parent with
      record
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Set_Image return Child'class is
      Local_Data : Child;
   begin
      Local_Data.Image := To_Unbounded_String ("Hello");
      return Local_Data;
   end Set_Image;

   procedure Proc1 is
      The_Data : Parent'class := Set_Image;
   begin
      Put_Line ("Child'Alignment =" & Child'Alignment'Img);
      Put_Line ("The_Data'Alignment =" & The_Data'Alignment'Img);
   end;

   procedure Proc2 is

      procedure Nested (X : Parent'Class) is
        The_Data : Parent'Class := X;
      begin
        Put_Line ("Child'Alignment =" & Child'Alignment'Img);
        Put_Line ("The_Data'Alignment =" & The_Data'Alignment'Img);
      end;

      The_Data : Parent'Class := Set_Image;

   begin
      Nested (The_Data);
   end;

end Derived_Type3_Pkg;
