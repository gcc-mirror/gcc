------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . S O C K E T S . T H I N _ C O M M O N             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2008-2019, AdaCore                   --
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
     (Sin     : Sockaddr_Access;
      Address : Sock_Addr_Type)
   is
   begin
      Set_Family (Sin.Sin_Family, Address.Family);
      Sin.Sin_Port := Short_To_Network (C.unsigned_short (Address.Port));

      case Address.Family is
         when Family_Inet =>
            Sin.Sin_Addr := To_In_Addr (Address.Addr);
         when Family_Inet6 =>
            Sin.Sin6_Addr := To_In6_Addr (Address.Addr);
            Sin.Sin6_Scope_Id := 0;
         when Family_Unspec =>
            null;
      end case;
   end Set_Address;

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Sin : Sockaddr) return Sock_Addr_Type is
      Family : constant C.unsigned_short :=
        (if SOSC.Has_Sockaddr_Len = 0 then Sin.Sin_Family.Short_Family
         else C.unsigned_short (Sin.Sin_Family.Char_Family));
      Result : Sock_Addr_Type
        (case Family is
            when SOSC.AF_INET6 => Family_Inet6,
            when SOSC.AF_INET  => Family_Inet,
            when others        => Family_Unspec);
   begin
      Result.Port := Port_Type (Network_To_Short (Sin.Sin_Port));

      case Result.Family is
         when Family_Inet =>
            To_Inet_Addr (Sin.Sin_Addr, Result.Addr);
         when Family_Inet6 =>
            To_Inet_Addr (Sin.Sin6_Addr, Result.Addr);
         when Family_Unspec =>
            Result.Addr := (Family => Family_Unspec);
      end case;

      return Result;
   end Get_Address;

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

   ----------------
   -- To_In_Addr --
   ----------------

   function To_In_Addr (Addr : Inet_Addr_Type) return In_Addr is
   begin
      if Addr.Family = Family_Inet then
         return (S_B1 => C.unsigned_char (Addr.Sin_V4 (1)),
                 S_B2 => C.unsigned_char (Addr.Sin_V4 (2)),
                 S_B3 => C.unsigned_char (Addr.Sin_V4 (3)),
                 S_B4 => C.unsigned_char (Addr.Sin_V4 (4)));
      end if;

      raise Socket_Error with "IPv6 not supported";
   end To_In_Addr;

   ------------------
   -- To_Inet_Addr --
   ------------------

   procedure To_Inet_Addr
     (Addr   : In_Addr;
      Result : out Inet_Addr_Type) is
   begin
      Result.Sin_V4 (1) := Inet_Addr_Comp_Type (Addr.S_B1);
      Result.Sin_V4 (2) := Inet_Addr_Comp_Type (Addr.S_B2);
      Result.Sin_V4 (3) := Inet_Addr_Comp_Type (Addr.S_B3);
      Result.Sin_V4 (4) := Inet_Addr_Comp_Type (Addr.S_B4);
   end To_Inet_Addr;

   ------------------
   -- To_Inet_Addr --
   ------------------

   procedure To_Inet_Addr
     (Addr   : In6_Addr;
      Result : out Inet_Addr_Type)
   is
      Sin_V6 : Inet_Addr_V6_Type;
   begin
      for J in Addr'Range loop
         Sin_V6 (J) := Inet_Addr_Comp_Type (Addr (J));
      end loop;

      Result := (Family => Family_Inet6, Sin_V6 => Sin_V6);
   end To_Inet_Addr;

   ----------------
   -- To_In_Addr --
   ----------------

   function To_In6_Addr (Addr : Inet_Addr_Type) return In6_Addr is
      Result : In6_Addr;
   begin
      for J in Addr.Sin_V6'Range loop
         Result (J) := C.unsigned_char (Addr.Sin_V6 (J));
      end loop;

      return Result;
   end To_In6_Addr;

   ----------------------
   -- Short_To_Network --
   ----------------------

   function Short_To_Network (S : C.unsigned_short) return C.unsigned_short is
      use Interfaces;
      use System;

   begin
      --  Big-endian case. No conversion needed. On these platforms, htons()
      --  defaults to a null procedure.

      if Default_Bit_Order = High_Order_First then
         return S;

      --  Little-endian case. We must swap the high and low bytes of this
      --  short to make the port number network compliant.

      else
         return C.unsigned_short (Rotate_Left (Unsigned_16 (S), 8));
      end if;
   end Short_To_Network;

end GNAT.Sockets.Thin_Common;
