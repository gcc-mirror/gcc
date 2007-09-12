------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . S T R I N G S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  Common String access types and related subprograms

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.String (file g-string.ads).

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

with Ada.Unchecked_Deallocation;

package System.Strings is
   pragma Preelaborate;

   type String_Access is access all String;
   --  General purpose string access type. Note that the caller is
   --  responsible for freeing allocated strings to avoid memory leaks.

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => String, Name => String_Access);
   --  This procedure is provided for freeing allocated values of type
   --  String_Access.

   type String_List is array (Positive range <>) of String_Access;
   type String_List_Access is access all String_List;
   --  General purpose array and pointer for list of string accesses

   procedure Free (Arg : in out String_List_Access);
   --  Frees the given array and all strings that its elements reference,
   --  and then sets the argument to null. Provided for freeing allocated
   --  values of this type.

end System.Strings;
