------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . P A C K _ 3 8                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1999 Free Software Foundation, Inc.          --
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

--  Handling of packed arrays with Component_Size = 38

package System.Pack_38 is
pragma Preelaborate (Pack_38);

   Bits : constant := 38;

   type Bits_38 is mod 2 ** Bits;
   for Bits_38'Size use Bits;

   function Get_38 (Arr : System.Address; N : Natural) return Bits_38;
   --  Arr is the address of the packed array, N is the zero-based
   --  subscript. This element is extracted and returned.

   procedure Set_38 (Arr : System.Address; N : Natural; E : Bits_38);
   --  Arr is the address of the packed array, N is the zero-based
   --  subscript. This element is set to the given value.

   function GetU_38 (Arr : System.Address; N : Natural) return Bits_38;
   --  Arr is the address of the packed array, N is the zero-based
   --  subscript. This element is extracted and returned. This version
   --  is used when Arr may represent an unaligned address.

   procedure SetU_38 (Arr : System.Address; N : Natural; E : Bits_38);
   --  Arr is the address of the packed array, N is the zero-based
   --  subscript. This element is set to the given value. This version
   --  is used when Arr may represent an unaligned address

end System.Pack_38;
