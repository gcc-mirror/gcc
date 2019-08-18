with Ada.Text_IO; use Ada.Text_IO;

procedure Enum_Val1 is
   type Enum is (Two, Four);
   for Enum use (2, 4);

   Count : Natural := 0;

begin
   for I in 10 .. 11 loop
      begin
         Put (Integer'Image (I) & ": ");
         Put_Line (Enum'Image (Enum'Enum_Val (I)));
      exception
         when Constraint_Error =>
            Count := Count + 1;
      end;
   end loop;
   if Count /= 2 then
      raise Program_Error;
   end if;
end;
