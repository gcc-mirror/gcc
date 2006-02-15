------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--     A D A . W I D E _ T E X T _ I O . W I D E _ B O U N D E D _ I O      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Wide_Bounded;

generic
   with package Wide_Bounded is
     new Ada.Strings.Wide_Bounded.Generic_Bounded_Length (<>);

package Ada.Wide_Text_IO.Wide_Bounded_IO is

   function Get_Line return Wide_Bounded.Bounded_Wide_String;

   function Get_Line
     (File : File_Type) return Wide_Bounded.Bounded_Wide_String;

   procedure Get_Line
     (Item : out Wide_Bounded.Bounded_Wide_String);

   procedure Get_Line
     (File : File_Type;
      Item : out Wide_Bounded.Bounded_Wide_String);

   procedure Put
     (Item : Wide_Bounded.Bounded_Wide_String);

   procedure Put
     (File : File_Type;
      Item : Wide_Bounded.Bounded_Wide_String);

   procedure Put_Line
     (Item : Wide_Bounded.Bounded_Wide_String);

   procedure Put_Line
     (File : File_Type;
      Item : Wide_Bounded.Bounded_Wide_String);

end Ada.Wide_Text_IO.Wide_Bounded_IO;
