------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S T O R A G E _ E L E M E N T S              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--        Copyright (C) 1992,1993,1994 Free Software Foundation, Inc.       --
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

with Unchecked_Conversion;
package body System.Storage_Elements is

   pragma Suppress (All_Checks);

   function To_Address is new Unchecked_Conversion (Storage_Offset, Address);
   function To_Offset  is new Unchecked_Conversion (Address, Storage_Offset);

   --  Address arithmetic

   function "+" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Left + To_Address (Right);
   end "+";

   function "+" (Left : Storage_Offset; Right : Address) return Address is
   begin
      return To_Address (Left) + Right;
   end "+";

   function "-" (Left : Address; Right : Storage_Offset) return Address is
   begin
      return Left - To_Address (Right);
   end "-";

   function "-" (Left, Right : Address) return Storage_Offset is
   begin
      return To_Offset (Left - Right);
   end "-";

   function "mod" (Left : Address; Right : Storage_Offset)
     return Storage_Offset is
   begin
      if Right >= 0 then
         return Storage_Offset (Address'(Left mod Address (Right)));
      else
         return -Storage_Offset (Address'(Left mod Address (-Right)));
      end if;
   end "mod";

   --  Conversion to/from integers

   function To_Address (Value : Integer_Address) return Address is
   begin
      return Address (Value);
   end To_Address;

   function To_Integer (Value : Address) return Integer_Address is
   begin
      return Integer_Address (Value);
   end To_Integer;

end System.Storage_Elements;
