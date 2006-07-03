-- { dg-do run }
-- { dg-options "-O" }

procedure string_slice is

   subtype Key_T is String (1 .. 3);

   function One_Xkey return Key_T is
      Key : Key_T := "XXX";
   begin
      Key (1 .. 2) := "__";
      return Key;
   end;

   Key : Key_T := One_Xkey;

begin
   if Key (3) /= 'X' then
      raise Program_Error;
   end if;
end;
