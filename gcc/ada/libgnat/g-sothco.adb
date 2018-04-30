------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . S O C K E T S . T H I N _ C O M M O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2008-2018, AdaCore                   --
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
