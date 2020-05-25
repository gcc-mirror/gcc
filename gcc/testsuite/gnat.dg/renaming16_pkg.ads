with Ada.Iterator_Interfaces;

package Renaming16_Pkg is

   type Results_Type is tagged null record;

   type Cursor is access constant Results_Type'Class;

   not overriding
   function Has_Element (Position : Cursor) return Boolean;

   package Base_Iterators is
      new Ada.Iterator_Interfaces (Cursor, Has_Element);

   -- Can be with null record
   type Bindings_Iterator is
     new Base_Iterators.Forward_Iterator with
      record
         Ref: Cursor;
      end record;

   not overriding
   function Create_Bindings_Iterator
     (Results : in out Results_Type'Class)
     return Bindings_Iterator;

   overriding function First (Object: Bindings_Iterator) return Cursor;
   overriding function Next  (Object: Bindings_Iterator; Position: Cursor) return Cursor;

   function Whatever return Boolean;
   subtype Bindings_Query_Results_Type is Results_Type
     with Dynamic_Predicate => Whatever;

end Renaming16_Pkg;
