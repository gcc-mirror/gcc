------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUNTIME COMPONENTS                         --
--                                                                          --
--        A D A . S T R I N G S . W I D E _ U N B O U N D E D . A U X       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

--  This child package of Ada.Strings.Wide_Unbounded provides some specialized
--  access functions which are intended to allow more efficient use of the
--  facilities of Ada.Strings.Wide_Unbounded, particularly by other layered
--  utilities.

package Ada.Strings.Wide_Unbounded.Aux is
pragma Preelaborate (Aux);

   function Get_Wide_String
     (U : Unbounded_Wide_String) return Wide_String_Access;
   pragma Inline (Get_Wide_String);
   --  This function returns the internal string pointer used in the
   --  representation of an unbounded string. There is no copy involved,
   --  so the value obtained references the same string as the original
   --  unbounded string. The characters of this string may not be modified
   --  via the returned pointer, and are valid only as long as the original
   --  unbounded string is not modified. Violating either of these two
   --  rules results in erroneous execution.
   --
   --  This function is much more efficient than the use of To_Wide_String
   --  since it avoids the need to copy the string. The lower bound of the
   --  referenced string returned by this call is always one.

   procedure Set_Wide_String
     (UP : in out Unbounded_Wide_String;
      S  : Wide_String);
   pragma Inline (Set_Wide_String);
   --  This function sets the string contents of the referenced unbounded
   --  string to the given string value. It is significantly more efficient
   --  than the use of To_Unbounded_Wide_String with an assignment, since it
   --  avoids the necessity of messing with finalization chains. The lower
   --  bound of the string S is not required to be one.

   procedure Set_Wide_String
     (UP : in out Unbounded_Wide_String;
      S  : Wide_String_Access);
   pragma Inline (Set_Wide_String);
   --  This version of Set_Wide_String takes a string access value, rather
   --  than string. The lower bound of the string value is required to be one,
   --  and this requirement is not checked.

end Ada.Strings.Wide_Unbounded.Aux;
