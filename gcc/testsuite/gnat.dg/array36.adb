--  { dg-do run }

procedure Array36 is

   subtype Str is String (1 .. 3);

   type Rec is record
      S : Str;
   end record;

   type T is record
      B : Boolean;
      R : Rec;
   end record;

   for T use record
      B at 0 range 0 .. 0;
      R at 0 range 1 .. 24;
   end record;

   X : T := (B => True, R => (S => "123"));

begin
   X.B := False;
   if X.R.S /= "123" then
      raise Program_Error;
   end if;
end;
