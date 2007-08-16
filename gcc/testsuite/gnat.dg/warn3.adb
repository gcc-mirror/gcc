--  { dg-do compile }
--  { dg-options "-gnatwu" }

with Ada.Command_Line; use Ada.Command_Line;
with Text_IO; use Text_IO;
procedure warn3 is
   type Weekdays is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
begin
   if Argument_Count > 0 then
      Put_Line
        (Argument (1) & " is weekday number"
         & Integer'Image
            (Weekdays'Pos (Weekdays'Value (Argument (1)))));
   end if;
end;
