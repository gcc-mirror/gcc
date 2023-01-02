------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--        A D A . S T R I N G S . W I D E _ U N B O U N D E D . A U X       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
   pragma Preelaborate;

   subtype Big_Wide_String is Wide_String (Positive'Range);
   type Big_Wide_String_Access is access all Big_Wide_String;

   procedure Get_Wide_String
     (U : Unbounded_Wide_String;
      S : out Big_Wide_String_Access;
      L : out Natural);
   pragma Inline (Get_Wide_String);
   --  This procedure returns the internal string pointer used in the
   --  representation of an unbounded string as well as the actual current
   --  length (which may be less than S.all'Length because in general there
   --  can be extra space assigned). The characters of this string may be
   --  not be modified via the returned pointer,  and are valid only as
   --  long as the original unbounded string is not accessed or modified.
   --
   --  This procedure is much more efficient than the use of To_Wide_String
   --  since it avoids the need to copy the string. The lower bound of the
   --  referenced string returned by this call is always one, so the actual
   --  string data is always accessible as S (1 .. L).

   procedure Set_Wide_String (UP : out Unbounded_Wide_String; S : Wide_String)
     renames Set_Unbounded_Wide_String;
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
