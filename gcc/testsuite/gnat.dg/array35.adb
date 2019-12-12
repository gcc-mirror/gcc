--  { dg-do run }

procedure Array35 is

   subtype Str is String (1 .. 3);

   type T is record
      B : Boolean;
      S : Str;
   end record;

   for T use record
      B at 0 range 0 .. 0;
      S at 0 range 1 .. 24;
   end record;

   X : T := (B => True, S => "123");

begin
   X.B := False;
   if X.S /= "123" then
      raise Program_Error;
   end if;
end;
