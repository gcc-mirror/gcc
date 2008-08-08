------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    G N A T . S O C K E T S . T H I N . T A S K _ S A F E _ N E T D B     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                  Copyright (C) 2007-2008, AdaCore                        --
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

--  This version is used on VxWorks. Note that the corresponding spec is in
--  g-sttsne-locking.ads.

with Ada.Unchecked_Conversion;
with Interfaces.C; use Interfaces.C;

package body GNAT.Sockets.Thin.Task_Safe_NetDB is

   --  The following additional data is returned by Safe_Gethostbyname
   --  and Safe_Getostbyaddr in the user provided buffer.

   type Netdb_Host_Data (Name_Length : C.size_t) is record
      Address   : aliased In_Addr;
      Addr_List : aliased In_Addr_Access_Array (0 .. 1);
      Name      : aliased C.char_array (0 .. Name_Length);
   end record;

   Alias_Access : constant Chars_Ptr_Pointers.Pointer :=
                    new C.Strings.chars_ptr'(C.Strings.Null_Ptr);
   --  Constant used to create a Hostent record manually

   ------------------------
   -- Safe_Gethostbyaddr --
   ------------------------

   function Safe_Gethostbyaddr
     (Addr      : System.Address;
      Addr_Len  : C.int;
      Addr_Type : C.int;
      Ret       : not null access Hostent;
      Buf       : System.Address;
      Buflen    : C.int;
      H_Errnop  : not null access C.int) return C.int
   is
      type int_Access is access int;
      function To_Pointer is
        new Ada.Unchecked_Conversion (System.Address, int_Access);

      function VxWorks_hostGetByAddr
        (Addr : C.int; Buf : System.Address) return C.int;
      pragma Import (C, VxWorks_hostGetByAddr, "hostGetByAddr");

      Netdb_Data : Netdb_Host_Data (Name_Length => Max_Name_Length);
      pragma Import (Ada, Netdb_Data);
      for Netdb_Data'Address use Buf;

      pragma Unreferenced (H_Errnop);
      --  VxWorks does not provide h_errno

   begin
      pragma Assert (Addr_Type = SOSC.AF_INET);
      pragma Assert (Addr_Len = In_Addr'Size / 8);

      --  Check that provided buffer is sufficiently large to hold the
      --  data we want to return.

      if Netdb_Data'Size / 8 > Buflen then
         return -1;
      end if;

      if VxWorks_hostGetByAddr (To_Pointer (Addr).all,
                                Netdb_Data.Name'Address)
           /= SOSC.OK
      then
         return -1;
      end if;

      Netdb_Data.Address   := To_In_Addr (To_Pointer (Addr).all);
      Netdb_Data.Addr_List :=
        (0 => Netdb_Data.Address'Unchecked_Access,
         1 => null);

      Ret.H_Name      := C.Strings.To_Chars_Ptr
                           (Netdb_Data.Name'Unrestricted_Access);
      Ret.H_Aliases   := Alias_Access;
      Ret.H_Addrtype  := SOSC.AF_INET;
      Ret.H_Length    := 4;
      Ret.H_Addr_List :=
        Netdb_Data.Addr_List (Netdb_Data.Addr_List'First)'Unchecked_Access;
      return 0;
   end Safe_Gethostbyaddr;

   ------------------------
   -- Safe_Gethostbyname --
   ------------------------

   function Safe_Gethostbyname
     (Name     : C.char_array;
      Ret      : not null access Hostent;
      Buf      : System.Address;
      Buflen   : C.int;
      H_Errnop : not null access C.int) return C.int
   is
      function VxWorks_hostGetByName
        (Name : C.char_array) return C.int;
      pragma Import (C, VxWorks_hostGetByName, "hostGetByName");

      Addr : C.int;

      pragma Unreferenced (H_Errnop);
      --  VxWorks does not provide h_errno

   begin
      Addr := VxWorks_hostGetByName (Name);
      if Addr = SOSC.ERROR then
         return -1;
      end if;

      declare
         Netdb_Data : Netdb_Host_Data (Name_Length => Name'Length);
         pragma Import (Ada, Netdb_Data);
         for Netdb_Data'Address use Buf;

      begin
         --  Check that provided buffer is sufficiently large to hold the
         --  data we want to return.

         if Netdb_Data'Size / 8 > Buflen then
            return -1;
         end if;

         Netdb_Data.Address   := To_In_Addr (Addr);
         Netdb_Data.Addr_List :=
           (0 => Netdb_Data.Address'Unchecked_Access,
            1 => null);
         Netdb_Data.Name (Netdb_Data.Name'First .. Name'Length - 1) := Name;

         Ret.H_Name      := C.Strings.To_Chars_Ptr
                              (Netdb_Data.Name'Unrestricted_Access);
         Ret.H_Aliases   := Alias_Access;
         Ret.H_Addrtype  := SOSC.AF_INET;
         Ret.H_Length    := 4;
         Ret.H_Addr_List :=
           Netdb_Data.Addr_List (Netdb_Data.Addr_List'First)'Unchecked_Access;
      end;
      return 0;
   end Safe_Gethostbyname;

   ------------------------
   -- Safe_Getservbyname --
   ------------------------

   function Safe_Getservbyname
     (Name     : C.char_array;
      Proto    : C.char_array;
      Ret      : not null access Servent;
      Buf      : System.Address;
      Buflen   : C.int) return C.int
   is
      pragma Unreferenced (Name, Proto, Ret, Buf, Buflen);
   begin
      --  Not available under VxWorks
      return -1;
   end Safe_Getservbyname;

   ------------------------
   -- Safe_Getservbyport --
   ------------------------

   function Safe_Getservbyport
     (Port     : C.int;
      Proto    : C.char_array;
      Ret      : not null access Servent;
      Buf      : System.Address;
      Buflen   : C.int) return C.int
   is
      pragma Unreferenced (Port, Proto, Ret, Buf, Buflen);
   begin
      --  Not available under VxWorks
      return -1;
   end Safe_Getservbyport;

end GNAT.Sockets.Thin.Task_Safe_NetDB;
