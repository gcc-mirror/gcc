------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . S T R I N G S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1995-2020, Free Software Foundation, Inc.         --
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

--  Common String access types and related subprograms

--  Note: this package is in the System hierarchy so that it can be directly
--  be used by other predefined packages. User access to this package is via
--  a renaming of this package in GNAT.String (file g-string.ads).

pragma Compiler_Unit_Warning;

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
