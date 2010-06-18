------------------------------------------------------------------------------
--                                                                          --
--                          GNAT RUN-TIME COMPONENTS                        --
--                                                                          --
--            A D A . S T R I N G S . U N B O U N D E D . A U X             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This child package of Ada.Strings.Unbounded provides some specialized
--  access functions which are intended to allow more efficient use of the
--  facilities of Ada.Strings.Unbounded, particularly by other layered
--  utilities (such as GNAT.SPITBOL.Patterns).

package Ada.Strings.Unbounded.Aux is
   pragma Preelaborate;

   subtype Big_String is String (1 .. Positive'Last);
   type Big_String_Access is access all Big_String;

   procedure Get_String
     (U : Unbounded_String;
      S : out Big_String_Access;
      L : out Natural);
   pragma Inline (Get_String);
   --  This procedure returns the internal string pointer used in the
   --  representation of an unbounded string as well as the actual current
   --  length (which may be less than S.all'Length because in general there
   --  can be extra space assigned). The characters of this string may be
   --  not be modified via the returned pointer,  and are valid only as
   --  long as the original unbounded string is not accessed or modified.
   --
   --  This procedure is much more efficient than the use of To_String
   --  since it avoids the need to copy the string. The lower bound of the
   --  referenced string returned by this call is always one, so the actual
   --  string data is always accessible as S (1 .. L).

   procedure Set_String (UP : out Unbounded_String; S : String)
     renames Set_Unbounded_String;
   --  This function is simply a renaming of the new Ada 2005 function as shown
   --  above. It is provided for historical reasons, but should be removed at
   --  this stage???

   procedure Set_String (UP : in out Unbounded_String; S : String_Access);
   pragma Inline (Set_String);
   --  This version of Set_Unbounded_String takes a string access value, rather
   --  than a string. The lower bound of the string value is required to be
   --  one, and this requirement is not checked.

end Ada.Strings.Unbounded.Aux;
