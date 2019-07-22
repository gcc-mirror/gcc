with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

procedure Encode_String1_Pkg (Val : S) is
begin
   declare
      Result : constant String := Encode (Val);
   begin
      Put_Line (Result);
   end;

exception
   when Ex : others =>
      Put_Line ("ERROR: Unexpected exception " & Exception_Name (Ex));
end;
