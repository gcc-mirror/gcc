with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Opt20 is

   type Build_Mode_State is (None, Static, Dynamic, Relocatable);

   procedure Build_Library (For_Project : Integer) is
      Project_Name : constant String := Get_Name_String (For_Project);
      The_Build_Mode : Build_Mode_State := None;
   begin
      Fail (Project_Name);
      Write_Str (To_Lower (Build_Mode_State'Image (The_Build_Mode)));
   end;

end Opt20;
