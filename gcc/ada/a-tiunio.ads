------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--              A D A . T E X T _ I O . U N B O U N D E D _ I O             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Note: historically GNAT provided these subprograms as a child of the
--  package Ada.Strings.Unbounded. So we implement this new Ada 2005 package
--  by renaming the subprograms in that child. This is a more straightforward
--  implementation anyway, since we need access to the internal representation
--  of Ada.Strings.Unbounded.Unbounded_String.


with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

package Ada.Text_IO.Unbounded_IO is

   procedure Put
     (File : File_Type;
      Item : Strings.Unbounded.Unbounded_String)
   renames Ada.Strings.Unbounded.Text_IO.Put;

   procedure Put
     (Item : Strings.Unbounded.Unbounded_String)
   renames Ada.Strings.Unbounded.Text_IO.Put;

   procedure Put_Line
     (File : Text_IO.File_Type;
      Item : Strings.Unbounded.Unbounded_String)
   renames Ada.Strings.Unbounded.Text_IO.Put_Line;

   procedure Put_Line
     (Item : Strings.Unbounded.Unbounded_String)
   renames Ada.Strings.Unbounded.Text_IO.Put_Line;

   function Get_Line
     (File : File_Type) return Strings.Unbounded.Unbounded_String
   renames Ada.Strings.Unbounded.Text_IO.Get_Line;

   function Get_Line return Strings.Unbounded.Unbounded_String
   renames Ada.Strings.Unbounded.Text_IO.Get_Line;

   procedure Get_Line
      (File : File_Type;
       Item : out Strings.Unbounded.Unbounded_String)
   renames Ada.Strings.Unbounded.Text_IO.Get_Line;

   procedure Get_Line
     (Item : out Strings.Unbounded.Unbounded_String)
   renames Ada.Strings.Unbounded.Text_IO.Get_Line;

end Ada.Text_IO.Unbounded_IO;
