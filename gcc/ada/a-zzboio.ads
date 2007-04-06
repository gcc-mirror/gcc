------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 ADA.WIDE_WIDE_TEXT_IO.WIDE_WIDE_BOUNDED_IO               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Wide_Bounded;

generic
   with package Wide_Wide_Bounded is
     new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length (<>);

package Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO is

   function Get_Line return Wide_Wide_Bounded.Bounded_Wide_Wide_String;

   function Get_Line
     (File : File_Type) return Wide_Wide_Bounded.Bounded_Wide_Wide_String;

   procedure Get_Line
     (Item : out Wide_Wide_Bounded.Bounded_Wide_Wide_String);

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_Wide_Bounded.Bounded_Wide_Wide_String);

   procedure Put
     (Item : Wide_Wide_Bounded.Bounded_Wide_Wide_String);

   procedure Put
     (File : File_Type;
      Item : Wide_Wide_Bounded.Bounded_Wide_Wide_String);

   procedure Put_Line
     (Item : Wide_Wide_Bounded.Bounded_Wide_Wide_String);

   procedure Put_Line
     (File : File_Type;
      Item : Wide_Wide_Bounded.Bounded_Wide_Wide_String);

end Ada.Wide_Wide_Text_IO.Wide_Wide_Bounded_IO;
