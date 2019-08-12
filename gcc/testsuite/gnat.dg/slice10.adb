--  { dg-do run }

procedure Slice10 is

   subtype Str is String (1 .. 3);

   type T is record
      B : Boolean;
      S : Str;
   end record;

   for T use record
      B at 0 range 0 .. 0;
      S at 0 range 1 .. 24;
   end record;

   function Match (X, Y: T; Length : Positive) return Boolean is
   begin
      return X.S (1 .. Length) = Y.S (1 .. Length);
   end;

   X, Y : T := (B => True, S => "123");

begin
   X.B := False;
   if not match (X, Y, 3) then
      raise Program_Error;
   end if;
end;
