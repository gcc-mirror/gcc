------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUNTIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                    S Y S T E M . S T R I N G _ O P S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2000 Free Software Foundation, Inc.          --
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

--  This package contains functions for runtime operations on strings

package System.String_Ops is
pragma Pure (String_Ops);

   function Str_Concat (X, Y : String) return String;
   --  Concatenate two strings and return resulting string

   function Str_Concat_SC (X : String; Y : Character) return String;
   --  Concatenate string and character

   function Str_Concat_CS (X : Character; Y : String) return String;
   --  Concatenate character and string

   function Str_Concat_CC (X, Y : Character) return String;
   --  Concatenate two characters

   function Str_Equal (A, B : String) return Boolean;
   --  Compare two strings for equality

   procedure Str_Normalize (A : in out String);
   --  Initialize String object if pragma Normalize_Scalars is in effect.

   procedure Wide_Str_Normalize (A : in out Wide_String);
   --  Ditto for Wide_String.

   pragma Inline (Str_Normalize);
   pragma Inline (Wide_Str_Normalize);
end System.String_Ops;
