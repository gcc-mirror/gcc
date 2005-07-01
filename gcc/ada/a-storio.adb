------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . S T O R A G E _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

with Unchecked_Conversion;

package body Ada.Storage_IO is

   type Buffer_Ptr is access all Buffer_Type;
   type Elmt_Ptr   is access all Element_Type;

   function To_Buffer_Ptr is new Unchecked_Conversion (Elmt_Ptr, Buffer_Ptr);

   ----------
   -- Read --
   ----------

   procedure Read (Buffer : Buffer_Type; Item : out Element_Type) is
   begin
      To_Buffer_Ptr (Item'Unrestricted_Access).all := Buffer;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Buffer : out Buffer_Type; Item : Element_Type) is
   begin
      Buffer := To_Buffer_Ptr (Item'Unrestricted_Access).all;
   end Write;

end Ada.Storage_IO;
