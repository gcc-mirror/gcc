------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 I N T E R F A C E S . C . S T R I N G S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1993-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

package Interfaces.C.Strings is
pragma Preelaborate (Strings);

   type char_array_access is access all char_array;

   type chars_ptr is private;

   type chars_ptr_array is array (size_t range <>) of chars_ptr;

   Null_Ptr : constant chars_ptr;

   function To_Chars_Ptr
     (Item      : in char_array_access;
      Nul_Check : in Boolean := False)
      return      chars_ptr;

   function New_Char_Array (Chars : in char_array) return chars_ptr;

   function New_String (Str : in String) return chars_ptr;

   procedure Free (Item : in out chars_ptr);

   Dereference_Error : exception;

   function Value (Item : in chars_ptr) return char_array;

   function Value
     (Item   : in chars_ptr;
      Length : in size_t)
      return   char_array;

   function Value (Item : in chars_ptr) return String;

   function Value
     (Item   : in chars_ptr;
      Length : in size_t)
      return   String;

   function Strlen (Item : in chars_ptr) return size_t;

   procedure Update
     (Item   : in chars_ptr;
      Offset : in size_t;
      Chars  : in char_array;
      Check  : Boolean := True);

   procedure Update
     (Item   : in chars_ptr;
      Offset : in size_t;
      Str    : in String;
      Check  : in Boolean := True);

   Update_Error : exception;

private
   type chars_ptr is access all Character;

   Null_Ptr : constant chars_ptr := null;
end Interfaces.C.Strings;
