------------------------------------------------------------------------------
--                                                                          --
--                   GNU ADA RUNTIME LIBRARY COMPONENTS                     --
--                                                                          --
--                S Y S T E M . S T R I N G _ C O M P A R E                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2002 Free Software Foundation, Inc.            --
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

--  This package contains functions for runtime comparisons on strings

package System.String_Compare is

   function Str_Compare
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural)
      return      Integer;
   --  Compare the string starting at address Left of length Left_Len
   --  with the string starting at address Right of length Right_Len.
   --  The comparison is in the normal Ada semantic sense of string
   --  comparison. The result is -1,0,+1 for Left<Right, Left=Right,
   --  Left>Right respectively. This function works with 4 byte words
   --  if the operands are aligned on 4-byte boundaries and long enough.

   function Str_Compare_Bytes
     (Left      : System.Address;
      Right     : System.Address;
      Left_Len  : Natural;
      Right_Len : Natural)
      return      Integer;
   --  Same functionality as Str_Compare but always proceeds by bytes.
   --  Used when the caller knows that the operands are unaligned, or
   --  short enough that it makes no sense to go by words.

end System.String_Compare;
