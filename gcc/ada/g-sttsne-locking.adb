------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--    G N A T . S O C K E T S . T H I N . T A S K _ S A F E _ N E T D B     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2007-2009, AdaCore                       --
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

--  This version is used on VMS and LynxOS

with GNAT.Task_Lock;

with Interfaces.C; use Interfaces.C;

package body GNAT.Sockets.Thin.Task_Safe_NetDB is

   --  The Safe_GetXXXbyYYY routines wrap the Nonreentrant_ versions using the
   --  task lock, and copy the relevant data structures (under the lock) into
   --  the result. The Nonreentrant_ versions are expected to be in the parent
   --  package GNAT.Sockets.Thin (on platforms that use this version of
   --  Task_Safe_NetDB).

   procedure Copy_Host_Entry
     (Source_Hostent       : Hostent;
      Target_Hostent       : out Hostent;
      Target_Buffer        : System.Address;
      Target_Buffer_Length : C.int;
      Result               : out C.int);
   --  Copy all the information from Source_Hostent into Target_Hostent,
   --  using Target_Buffer to store associated data.
   --  0 is returned on success, -1 on failure (in case the provided buffer
   --  is too small for the associated data).

   procedure Copy_Service_Entry
     (Source_Servent       : Servent;
      Target_Servent       : out Servent;
      Target_Buffer        : System.Address;
      Target_Buffer_Length : C.int;
      Result               : out C.int);
   --  Copy all the information from Source_Servent into Target_Servent,
   --  using Target_Buffer to store associated data.
   --  0 is returned on success, -1 on failure (in case the provided buffer
   --  is too small for the associated data).

   procedure Store_Name
     (Name          : char_array;
      Storage       : in out char_array;
      Storage_Index : in out size_t;
      Stored_Name   : out C.Strings.chars_ptr);
   --  Store the given Name at the first available location in Storage
   --  (indicated by Storage_Index, which is updated afterwards), and return
   --  the address of that location in Stored_Name.
   --  (Supporting routine for the two below).

   ---------------------
   -- Copy_Host_Entry --
   ---------------------

   procedure Copy_Host_Entry
     (Source_Hostent       : Hostent;
      Target_Hostent       : out Hostent;
      Target_Buffer        : System.Address;
      Target_Buffer_Length : C.int;
      Result               : out C.int)
   is
      use type C.Strings.chars_ptr;

      Names_Length : size_t;

      Source_Aliases : Chars_Ptr_Array
        renames Chars_Ptr_Pointers.Value
          (Source_Hostent.H_Aliases, Terminator => C.Strings.Null_Ptr);
      --  Null-terminated list of aliases (last element of this array is
      --  Null_Ptr).

      Source_Addresses : In_Addr_Access_Array
        renames In_Addr_Access_Pointers.Value
          (Source_Hostent.H_Addr_List, Terminator => null);

   begin
      Result := -1;
      Names_Length := C.Strings.Strlen (Source_Hostent.H_Name) + 1;

      for J in Source_Aliases'Range loop
         if Source_Aliases (J) /= C.Strings.Null_Ptr then
            Names_Length :=
              Names_Length + C.Strings.Strlen (Source_Aliases (J)) + 1;
         end if;
      end loop;

      declare
         type In_Addr_Array is array (Source_Addresses'Range)
                                 of aliased In_Addr;

         type Netdb_Host_Data is record
            Aliases_List   : aliased Chars_Ptr_Array (Source_Aliases'Range);
            Names          : aliased char_array (1 .. Names_Length);

            Addresses_List : aliased In_Addr_Access_Array
                                       (In_Addr_Array'Range);
            Addresses : In_Addr_Array;
            --  ??? This assumes support only for Inet family

         end record;

         Netdb_Data : Netdb_Host_Data;
         pragma Import (Ada, Netdb_Data);
         for Netdb_Data'Address use Target_Buffer;

         Names_Index : size_t := Netdb_Data.Names'First;
         --  Index of first available location in Netdb_Data.Names

      begin
         if Netdb_Data'Size / 8 > Target_Buffer_Length then
            return;
         end if;

         --  Copy host name

         Store_Name
           (C.Strings.Value (Source_Hostent.H_Name),
            Netdb_Data.Names, Names_Index,
            Target_Hostent.H_Name);

         --  Copy aliases (null-terminated string pointer array)

         Target_Hostent.H_Aliases :=
           Netdb_Data.Aliases_List
             (Netdb_Data.Aliases_List'First)'Unchecked_Access;
         for J in Netdb_Data.Aliases_List'Range loop
            if J = Netdb_Data.Aliases_List'Last then
               Netdb_Data.Aliases_List (J) := C.Strings.Null_Ptr;
            else
               Store_Name
                 (C.Strings.Value (Source_Aliases (J)),
                  Netdb_Data.Names, Names_Index,
                  Netdb_Data.Aliases_List (J));
            end if;
         end loop;

         --  Copy address type and length

         Target_Hostent.H_Addrtype := Source_Hostent.H_Addrtype;
         Target_Hostent.H_Length   := Source_Hostent.H_Length;

         --  Copy addresses

         Target_Hostent.H_Addr_List :=
           Netdb_Data.Addresses_List
             (Netdb_Data.Addresses_List'First)'Unchecked_Access;

         for J in Netdb_Data.Addresses'Range loop
            if J = Netdb_Data.Addresses'Last then
               Netdb_Data.Addresses_List (J) := null;
            else
               Netdb_Data.Addresses_List (J) :=
                 Netdb_Data.Addresses (J)'Unchecked_Access;

               Netdb_Data.Addresses (J) := Source_Addresses (J).all;
            end if;
         end loop;
      end;

      Result := 0;
   end Copy_Host_Entry;

   ------------------------
   -- Copy_Service_Entry --
   ------------------------

   procedure Copy_Service_Entry
     (Source_Servent       : Servent;
      Target_Servent       : out Servent;
      Target_Buffer        : System.Address;
      Target_Buffer_Length : C.int;
      Result               : out C.int)
   is
      use type C.Strings.chars_ptr;

      Names_Length : size_t;

      Source_Aliases : Chars_Ptr_Array
        renames Chars_Ptr_Pointers.Value
          (Servent_S_Aliases (Source_Servent),
           Terminator => C.Strings.Null_Ptr);
      --  Null-terminated list of aliases (last element of this array is
      --  Null_Ptr).

   begin
      Result := -1;
      Names_Length := C.Strings.Strlen (Servent_S_Name (Source_Servent)) + 1 +
                      C.Strings.Strlen (Servent_S_Proto (Source_Servent)) + 1;

      for J in Source_Aliases'Range loop
         if Source_Aliases (J) /= C.Strings.Null_Ptr then
            Names_Length :=
              Names_Length + C.Strings.Strlen (Source_Aliases (J)) + 1;
         end if;
      end loop;

      declare
         type Netdb_Service_Data is record
            Aliases_List : aliased Chars_Ptr_Array (Source_Aliases'Range);
            Names        : aliased char_array (1 .. Names_Length);
         end record;

         Netdb_Data : Netdb_Service_Data;
         pragma Import (Ada, Netdb_Data);
         for Netdb_Data'Address use Target_Buffer;

         Names_Index : size_t := Netdb_Data.Names'First;
         --  Index of first available location in Netdb_Data.Names

         Stored_Name : C.Strings.chars_ptr;

      begin
         if Netdb_Data'Size / 8 > Target_Buffer_Length then
            return;
         end if;

         --  Copy service name

         Store_Name
           (C.Strings.Value (Servent_S_Name (Source_Servent)),
            Netdb_Data.Names, Names_Index,
            Stored_Name);
         Servent_Set_S_Name (Target_Servent, Stored_Name);

         --  Copy aliases (null-terminated string pointer array)

         Servent_Set_S_Aliases
           (Target_Servent,
            Netdb_Data.Aliases_List
              (Netdb_Data.Aliases_List'First)'Unchecked_Access);

         --  Copy port number

         Servent_Set_S_Port (Target_Servent, Servent_S_Port (Source_Servent));

         --  Copy protocol name

         Store_Name
           (C.Strings.Value (Servent_S_Proto (Source_Servent)),
            Netdb_Data.Names, Names_Index,
            Stored_Name);
         Servent_Set_S_Proto (Target_Servent, Stored_Name);

         for J in Netdb_Data.Aliases_List'Range loop
            if J = Netdb_Data.Aliases_List'Last then
               Netdb_Data.Aliases_List (J) := C.Strings.Null_Ptr;
            else
               Store_Name
                 (C.Strings.Value (Source_Aliases (J)),
                  Netdb_Data.Names, Names_Index,
                  Netdb_Data.Aliases_List (J));
            end if;
         end loop;
      end;

      Result := 0;
   end Copy_Service_Entry;

   ------------------------
   -- Safe_Gethostbyaddr --
   ------------------------

   function Safe_Gethostbyaddr
     (Addr      : System.Address;
      Addr_Len  : C.int;
      Addr_Type : C.int;
      Ret      : not null access Hostent;
      Buf      : System.Address;
      Buflen   : C.int;
      H_Errnop : not null access C.int) return C.int
   is
      HE     : Hostent_Access;
      Result : C.int;
   begin
      Result := -1;
      GNAT.Task_Lock.Lock;
      HE := Nonreentrant_Gethostbyaddr (Addr, Addr_Len, Addr_Type);

      if HE = null then
         H_Errnop.all := C.int (Host_Errno);
         goto Unlock_Return;
      end if;

      --  Now copy the data to the user-provided buffer

      Copy_Host_Entry
        (Source_Hostent       => HE.all,
         Target_Hostent       => Ret.all,
         Target_Buffer        => Buf,
         Target_Buffer_Length => Buflen,
         Result               => Result);

      <<Unlock_Return>>
      GNAT.Task_Lock.Unlock;
      return Result;
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
      HE     : Hostent_Access;
      Result : C.int;
   begin
      Result := -1;
      GNAT.Task_Lock.Lock;
      HE := Nonreentrant_Gethostbyname (Name);

      if HE = null then
         H_Errnop.all := C.int (Host_Errno);
         goto Unlock_Return;
      end if;

      --  Now copy the data to the user-provided buffer

      Copy_Host_Entry
        (Source_Hostent       => HE.all,
         Target_Hostent       => Ret.all,
         Target_Buffer        => Buf,
         Target_Buffer_Length => Buflen,
         Result               => Result);

      <<Unlock_Return>>
      GNAT.Task_Lock.Unlock;
      return Result;
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
      SE     : Servent_Access;
      Result : C.int;
   begin
      Result := -1;
      GNAT.Task_Lock.Lock;
      SE := Nonreentrant_Getservbyname (Name, Proto);

      if SE = null then
         goto Unlock_Return;
      end if;

      --  Now copy the data to the user-provided buffer

      Copy_Service_Entry
        (Source_Servent       => SE.all,
         Target_Servent       => Ret.all,
         Target_Buffer        => Buf,
         Target_Buffer_Length => Buflen,
         Result               => Result);

      <<Unlock_Return>>
      GNAT.Task_Lock.Unlock;
      return Result;
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
      SE     : Servent_Access;
      Result : C.int;

   begin
      Result := -1;
      GNAT.Task_Lock.Lock;
      SE := Nonreentrant_Getservbyport (Port, Proto);

      if SE = null then
         goto Unlock_Return;
      end if;

      --  Now copy the data to the user-provided buffer

      Copy_Service_Entry
        (Source_Servent       => SE.all,
         Target_Servent       => Ret.all,
         Target_Buffer        => Buf,
         Target_Buffer_Length => Buflen,
         Result               => Result);

      <<Unlock_Return>>
      GNAT.Task_Lock.Unlock;
      return Result;
   end Safe_Getservbyport;

   ----------------
   -- Store_Name --
   ----------------

   procedure Store_Name
     (Name          : char_array;
      Storage       : in out char_array;
      Storage_Index : in out size_t;
      Stored_Name   : out C.Strings.chars_ptr)
   is
      First : constant C.size_t := Storage_Index;
      Last  : constant C.size_t := Storage_Index + Name'Length - 1;
   begin
      Storage (First .. Last) := Name;
      Stored_Name := C.Strings.To_Chars_Ptr
                       (Storage (First .. Last)'Unrestricted_Access);
      Storage_Index := Last + 1;
   end Store_Name;

end GNAT.Sockets.Thin.Task_Safe_NetDB;
