------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . S O C K E T S . T H I N _ C O M M O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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

package body GNAT.Sockets.Thin_Common is

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Sin     : Sockaddr_In_Access;
      Address : In_Addr)
   is
   begin
      Sin.Sin_Addr := Address;
   end Set_Address;

   ----------------
   -- Set_Family --
   ----------------

   procedure Set_Family
     (Length_And_Family : out Sockaddr_Length_And_Family;
      Family            : Family_Type)
   is
      C_Family : C.int renames Families (Family);
      Has_Sockaddr_Len : constant Boolean := SOSC.Has_Sockaddr_Len /= 0;
   begin
      if Has_Sockaddr_Len then
         Length_And_Family.Length       := Lengths (Family);
         Length_And_Family.Char_Family  := C.unsigned_char  (C_Family);
      else
         Length_And_Family.Short_Family := C.unsigned_short (C_Family);
      end if;
   end Set_Family;

   --------------
   -- Set_Port --
   --------------

   procedure Set_Port
     (Sin  : Sockaddr_In_Access;
      Port : C.unsigned_short)
   is
   begin
      Sin.Sin_Port := Port;
   end Set_Port;

end GNAT.Sockets.Thin_Common;
