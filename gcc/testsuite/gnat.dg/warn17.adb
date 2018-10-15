--  { dg-do compile }
--  { dg-options "-Wall" }

with Ada.Iterator_Interfaces;

procedure Warn17 is

   type Cursor is null record;

   function Has_Element (Position : Cursor) return Boolean;

   function Has_Element (Position : Cursor) return Boolean is (True);

   package My_Iterator is
     new Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Iterator is abstract new My_Iterator.Forward_Iterator with null record;

   pragma Unreferenced (Iterator);
begin
   null;
end Warn17;
