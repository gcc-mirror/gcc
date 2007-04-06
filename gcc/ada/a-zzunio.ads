------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               ADA.WIDE_WIDE_TEXT_IO.WIDE_WIDE_UNBOUNDED_IO               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  Note: historically GNAT provided these subprograms as a child of the
--  package Ada.Strings.Wide_Wide_Unbounded. So we implement this new Ada 2005
--  package by renaming the subprograms in that child. This is a more
--  straightforward implementation anyway, since we need access to the
--  internal representation of Unbounded_Wide_Wide_String.

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

package Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO is

   procedure Put
     (File : File_Type;
      Item : Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put;

   procedure Put
     (Item : Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put;

   procedure Put_Line
     (File : Wide_Wide_Text_IO.File_Type;
      Item : Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put_Line;

   procedure Put_Line
     (Item : Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Put_Line;

   function Get_Line
     (File : File_Type)
      return Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Get_Line;

   function Get_Line
     return Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Get_Line;

   procedure Get_Line
      (File : File_Type;
       Item : out Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Get_Line;

   procedure Get_Line
     (Item : out Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String)
   renames Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO.Get_Line;

end Ada.Wide_Wide_Text_IO.Wide_Wide_Unbounded_IO;
