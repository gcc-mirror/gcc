------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       A D A . S T O R A G E _ I O                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Conversion;

package body Ada.Storage_IO is

   type Buffer_Ptr is access all Buffer_Type;
   type Elmt_Ptr   is access all Element_Type;

   function To_Buffer_Ptr is
     new Ada.Unchecked_Conversion (Elmt_Ptr, Buffer_Ptr);

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
