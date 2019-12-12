--  { dg-do compile }
--  { dg-options "-gnatwa" }

with Ada.Text_IO;

procedure Warn22
is
   type X is
      record
         Str : String (1 .. 3);
      end record;

   type T is
      record
         Value : X;
      end record;

   procedure Consume_Data (Item : out T) is
   begin
      Item := (Value => (Str => "Bar"));
   end Consume_Data;

   Baz : T;
begin

   Baz := (Value => (Str => "Foo"));

   Ada.Text_IO.Put_Line (Baz.Value.Str);

   Consume_Data (Baz);

   Ada.Text_IO.Put_Line (Baz.Value.Str);

end Warn22;
