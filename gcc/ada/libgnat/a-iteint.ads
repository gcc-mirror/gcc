------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--               A D A . I T E R A T O R . I N T E R F A C E S              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

generic
   type Cursor;
   with function Has_Element (Position : Cursor) return Boolean;
   pragma Unreferenced (Has_Element);

package Ada.Iterator_Interfaces is
   pragma Pure;

   type Forward_Iterator is limited interface with No_Task_Parts;

   function First
     (Object : Forward_Iterator) return Cursor is abstract;
   function Next
     (Object   : Forward_Iterator;
      Position : Cursor) return Cursor is abstract;

   type Reversible_Iterator is limited interface and Forward_Iterator with
     No_Task_Parts;

   function Last
     (Object : Reversible_Iterator) return Cursor is abstract;
   function Previous
     (Object   : Reversible_Iterator;
      Position : Cursor) return Cursor is abstract;
end Ada.Iterator_Interfaces;
