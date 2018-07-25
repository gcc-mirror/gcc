--  { dg-do run }

procedure Equal2 is

   package L1 is
      type T is private;
      overriding function "=" (Left, Right : T) return Boolean;
   private
      type T is tagged record
        I : Integer := 0;
      end record;
   end L1;

   package L2 is
      type T is private;
   private
      type T is new L1.T;
      overriding function "=" (Left, Right : T) return Boolean;
   end L2;

   package body L1 is
      overriding function "=" (Left, Right : T) return Boolean is
      begin
        return False;
      end "=";
   end L1;

   package body L2 is
      overriding function "=" (Left, Right : T) return Boolean is
      begin
        return True;
      end "=";
   end L2;

   use type L2.T;
   Left, Right : L2.T;
begin
   if Left /= Right then
      raise Program_Error;
   end if;
end;
