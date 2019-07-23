--  { dg-do compile }

with Ada.Iterator_Interfaces;

procedure Iter6 is
  package Pkg is
  type Item (<>) is limited private;
   private

   type Cursor is null record;

  function Constant_Reference (The_Item     : aliased Item;
                               Unused_Index : Cursor) return String
     is ("");

  function Has_More (Data : Cursor) return Boolean is (False);

  package List_Iterator_Interfaces is new Ada.Iterator_Interfaces
    (Cursor, Has_More);

   function Iterate (The_Item : Item)
		    return List_Iterator_Interfaces.Forward_Iterator'class
     is (raise Program_Error);

  type Item (Name_Length : Natural) is tagged limited record
     null;
  end record
  with
    Constant_Indexing => Constant_Reference,
    Default_Iterator  => Iterate,
    Iterator_Element  => String;
  end Pkg; use Pkg;

  type Item_Ref is access Item;
  function F return Item_Ref is (null);
begin
   for I of F.all loop --  { dg-error "cannot iterate over \"Item\"" }
     null;
   end loop;
end;
