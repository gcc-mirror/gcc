------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--             ADA.STRINGS.WIDE_WIDE_UNBOUNDED.WIDE_WIDE_TEXT_IO            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1997-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This child package of Ada.Strings.Wide_Wide_Unbounded provides specialized
--  Wide_Wide_Text_IO routines that work directly with unbounded wide wide
--  strings, avoiding the inefficiencies of access via the standard interface,
--  and also taking direct advantage of the variable length semantics of these
--  strings.

with Ada.Wide_Wide_Text_IO;

package Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO is

   function Get_Line
     return Unbounded_Wide_Wide_String;
   function Get_Line
     (File : Ada.Wide_Wide_Text_IO.File_Type)
      return Unbounded_Wide_Wide_String;
   --  Reads up to the end of the current line, returning the result
   --  as an unbounded string of appropriate length. If no File parameter
   --  is present, input is from Current_Input.

   procedure Get_Line
     (File : Ada.Wide_Wide_Text_IO.File_Type;
      Item : out Unbounded_Wide_Wide_String);
   procedure Get_Line (Item : out Unbounded_Wide_Wide_String);
   --  Similar to the above, but in procedure form with an out parameter

   procedure Put
     (U : Unbounded_Wide_Wide_String);
   procedure Put
     (File : Ada.Wide_Wide_Text_IO.File_Type;
      U    : Unbounded_Wide_Wide_String);
   procedure Put_Line
     (U    : Unbounded_Wide_Wide_String);
   procedure Put_Line
     (File : Ada.Wide_Wide_Text_IO.File_Type;
      U    : Unbounded_Wide_Wide_String);
   --  These are equivalent to the standard Wide_Wide_Text_IO routines passed
   --  the value To_Wide_Wide_String (U), but operate more efficiently,
   --  because the extra copy of the argument is avoided.

end Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
