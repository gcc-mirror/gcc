-- { dg-do compile }

with Ada.Text_IO; use Ada.Text_IO;

procedure Concat4 (X : Integer) is
   Ximg : constant String := Integer'Image (X);
begin
   if X > 0 then
      Put_Line (Ximg & " is Positive");
   elsif X < 0 then
      Put_Line (Ximg & " is Negative");
   else
      Put_Line (Ximg & " is Null");
   end if;
end;

-- { dg-final { scan-assembler-not "_Unwind_Resume" } }
