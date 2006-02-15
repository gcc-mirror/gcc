------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--               A D A . T E X T _ I O . B O U N D E D _ I O                --
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

with Ada.Strings.Bounded;

generic
   with package Bounded is
     new Ada.Strings.Bounded.Generic_Bounded_Length (<>);

package Ada.Text_IO.Bounded_IO is

   function Get_Line return Bounded.Bounded_String;

   function Get_Line
     (File : File_Type) return Bounded.Bounded_String;

   procedure Get_Line
     (Item : out Bounded.Bounded_String);

   procedure Get_Line
     (File : File_Type;
      Item : out Bounded.Bounded_String);

   procedure Put
     (Item : Bounded.Bounded_String);

   procedure Put
     (File : File_Type;
      Item : Bounded.Bounded_String);

   procedure Put_Line
     (Item : Bounded.Bounded_String);

   procedure Put_Line
     (File : File_Type;
      Item : Bounded.Bounded_String);

end Ada.Text_IO.Bounded_IO;
