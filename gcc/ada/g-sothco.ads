------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . S O C K E T S . T H I N _ C O M M O N             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

--  This is the target-independent part of the thin sockets mapping.
--  This package should not be directly with'ed by an applications program.

with Ada.Unchecked_Conversion;

with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

package GNAT.Sockets.Thin_Common is

   package C renames Interfaces.C;

   use type C.int;
   --  This is so we can declare the Failure constant below

   Success : constant C.int :=  0;
   Failure : constant C.int := -1;

   type time_t is
     range -2 ** (8 * SOSC.SIZEOF_tv_sec - 1)
         .. 2 ** (8 * SOSC.SIZEOF_tv_sec - 1) - 1;
   for time_t'Size use 8 * SOSC.SIZEOF_tv_sec;
   pragma Convention (C, time_t);

   type suseconds_t is
     range -2 ** (8 * SOSC.SIZEOF_tv_usec - 1)
         .. 2 ** (8 * SOSC.SIZEOF_tv_usec - 1) - 1;
   for suseconds_t'Size use 8 * SOSC.SIZEOF_tv_usec;
   pragma Convention (C, suseconds_t);

   type Timeval is record
      Tv_Sec  : time_t;
      Tv_Usec : suseconds_t;
   end record;
   pragma Convention (C, Timeval);

   type Timeval_Access is access all Timeval;
   pragma Convention (C, Timeval_Access);

   Immediat : constant Timeval := (0, 0);

   -------------------------------------------
   -- Mapping tables to low level constants --
   -------------------------------------------

   Families : constant array (Family_Type) of C.int :=
                (Family_Inet  => SOSC.AF_INET,
                 Family_Inet6 => SOSC.AF_INET6);

   Lengths  : constant array (Family_Type) of C.unsigned_char :=
                (Family_Inet  => SOSC.SIZEOF_sockaddr_in,
                 Family_Inet6 => SOSC.SIZEOF_sockaddr_in6);

   ----------------------------
   -- Generic socket address --
   ----------------------------

   --  Common header

   --  All socket address types (struct sockaddr, struct sockaddr_storage,
   --  and protocol specific address types) start with the same 2-byte header,
   --  which is either a length and a family (one byte each) or just a two-byte
   --  family. The following unchecked union describes the two possible layouts
   --  and is meant to be constrained with SOSC.Have_Sockaddr_Len.

   type Sockaddr_Length_And_Family
     (Has_Sockaddr_Len : Boolean := False)
   is record
      case Has_Sockaddr_Len is
         when True =>
            Length      : C.unsigned_char;
            Char_Family : C.unsigned_char;

         when False =>
            Short_Family : C.unsigned_short;
      end case;
   end record;
   pragma Unchecked_Union (Sockaddr_Length_And_Family);
   pragma Convention (C, Sockaddr_Length_And_Family);

   procedure Set_Family
     (Length_And_Family : out Sockaddr_Length_And_Family;
      Family            : Family_Type);
   --  Set the family component to the appropriate value for Family, and also
   --  set Length accordingly if applicable on this platform.

   type Sockaddr is record
      Sa_Family : Sockaddr_Length_And_Family;
      --  Address family (and address length on some platforms)

      Sa_Data : C.char_array (1 .. 14) := (others => C.nul);
      --  Family-specific data
      --  Note that some platforms require that all unused (reserved) bytes
      --  in addresses be initialized to 0 (e.g. VxWorks).
   end record;
   pragma Convention (C, Sockaddr);
   --  Generic socket address

   type Sockaddr_Access is access all Sockaddr;
   pragma Convention (C, Sockaddr_Access);
   --  Access to socket address

   ----------------------------
   -- AF_INET socket address --
   ----------------------------

   type In_Addr is record
      S_B1, S_B2, S_B3, S_B4 : C.unsigned_char;
   end record;
   for In_Addr'Alignment use C.int'Alignment;
   pragma Convention (C, In_Addr);
   --  IPv4 address, represented as a network-order C.int. Note that the
   --  underlying operating system may assume that values of this type have
   --  C.int alignment, so we need to provide a suitable alignment clause here.

   function To_In_Addr is new Ada.Unchecked_Conversion (C.int, In_Addr);
   function To_Int     is new Ada.Unchecked_Conversion (In_Addr, C.int);

   type In_Addr_Access is access all In_Addr;
   pragma Convention (C, In_Addr_Access);
   --  Access to internet address

   Inaddr_Any : aliased constant In_Addr := (others => 0);
   --  Any internet address (all the interfaces)

   type In_Addr_Access_Array is array (C.size_t range <>)
     of aliased In_Addr_Access;
   pragma Convention (C, In_Addr_Access_Array);

   package In_Addr_Access_Pointers is new C.Pointers
     (C.size_t, In_Addr_Access, In_Addr_Access_Array, null);
   --  Array of internet addresses

   type Sockaddr_In is record
      Sin_Family : Sockaddr_Length_And_Family;
      --  Address family (and address length on some platforms)

      Sin_Port : C.unsigned_short;
      --  Port in network byte order

      Sin_Addr : In_Addr;
      --  IPv4 address

      Sin_Zero : C.char_array (1 .. 8) := (others => C.nul);
      --  Padding
      --
      --  Note that some platforms require that all unused (reserved) bytes
      --  in addresses be initialized to 0 (e.g. VxWorks).
   end record;
   pragma Convention (C, Sockaddr_In);
   --  Internet socket address

   type Sockaddr_In_Access is access all Sockaddr_In;
   pragma Convention (C, Sockaddr_In_Access);
   --  Access to internet socket address

   procedure Set_Port
     (Sin  : Sockaddr_In_Access;
      Port : C.unsigned_short);
   pragma Inline (Set_Port);
   --  Set Sin.Sin_Port to Port

   procedure Set_Address
     (Sin     : Sockaddr_In_Access;
      Address : In_Addr);
   pragma Inline (Set_Address);
   --  Set Sin.Sin_Addr to Address

   ---------------------
   -- Service entries --
   ---------------------

   type Chars_Ptr_Array is array (C.size_t range <>) of
     aliased C.Strings.chars_ptr;

   package Chars_Ptr_Pointers is
      new C.Pointers (C.size_t, C.Strings.chars_ptr, Chars_Ptr_Array,
                      C.Strings.Null_Ptr);
   --  Arrays of C (char *)

   type Servent is new
     System.Storage_Elements.Storage_Array (1 .. SOSC.SIZEOF_struct_servent);
   for Servent'Alignment use 8;
   --  Service entry. This is an opaque type used only via the following
   --  accessor functions, because 'struct servent' has different layouts on
   --  different platforms.

   type Servent_Access is access all Servent;
   pragma Convention (C, Servent_Access);
   --  Access to service entry

   function Servent_S_Name
     (E : Servent_Access) return C.Strings.chars_ptr;
   function Servent_S_Aliases
     (E : Servent_Access) return Chars_Ptr_Pointers.Pointer;
   function Servent_S_Port
     (E : Servent_Access) return C.int;
   function Servent_S_Proto
     (E : Servent_Access) return C.Strings.chars_ptr;

   ------------------
   -- Host entries --
   ------------------

   type Hostent is record
      H_Name      : C.Strings.chars_ptr;
      H_Aliases   : Chars_Ptr_Pointers.Pointer;
      H_Addrtype  : SOSC.H_Addrtype_T;
      H_Length    : SOSC.H_Length_T;
      H_Addr_List : In_Addr_Access_Pointers.Pointer;
   end record;
   pragma Convention (C, Hostent);
   --  Host entry

   type Hostent_Access is access all Hostent;
   pragma Convention (C, Hostent_Access);
   --  Access to host entry

   ------------------------------------
   -- Scatter/gather vector handling --
   ------------------------------------

   type Msghdr is record
      Msg_Name       : System.Address;
      Msg_Namelen    : C.unsigned;
      Msg_Iov        : System.Address;
      Msg_Iovlen     : SOSC.Msg_Iovlen_T;
      Msg_Control    : System.Address;
      Msg_Controllen : C.size_t;
      Msg_Flags      : C.int;
   end record;
   pragma Convention (C, Msghdr);

   ----------------------------
   -- Socket sets management --
   ----------------------------

   procedure Get_Socket_From_Set
     (Set    : access Fd_Set;
      Last   : access C.int;
      Socket : access C.int);
   --  Get last socket in Socket and remove it from the socket set. The
   --  parameter Last is a maximum value of the largest socket. This hint is
   --  used to avoid scanning very large socket sets. After a call to
   --  Get_Socket_From_Set, Last is set back to the real largest socket in the
   --  socket set.

   procedure Insert_Socket_In_Set
     (Set    : access Fd_Set;
      Socket : C.int);
   --  Insert socket in the socket set

   function  Is_Socket_In_Set
     (Set    : access constant Fd_Set;
      Socket : C.int) return C.int;
   --  Check whether Socket is in the socket set, return a non-zero
   --  value if it is, zero if it is not.

   procedure Last_Socket_In_Set
     (Set  : access Fd_Set;
      Last : access C.int);
   --  Find the largest socket in the socket set. This is needed for select().
   --  When Last_Socket_In_Set is called, parameter Last is a maximum value of
   --  the largest socket. This hint is used to avoid scanning very large
   --  socket sets. After the call, Last is set back to the real largest socket
   --  in the socket set.

   procedure Remove_Socket_From_Set (Set : access Fd_Set; Socket : C.int);
   --  Remove socket from the socket set

   procedure Reset_Socket_Set (Set : access Fd_Set);
   --  Make Set empty

   ------------------------------------------
   -- Pairs of signalling file descriptors --
   ------------------------------------------

   type Two_Ints is array (0 .. 1) of C.int;
   pragma Convention (C, Two_Ints);
   --  Container for two int values

   subtype Fd_Pair is Two_Ints;
   --  Two_Ints as used for Create_Signalling_Fds: a pair of connected file
   --  descriptors, one of which (the "read end" of the connection) being used
   --  for reading, the other one (the "write end") being used for writing.

   Read_End  : constant := 0;
   Write_End : constant := 1;
   --  Indices into an Fd_Pair value providing access to each of the connected
   --  file descriptors.

   function Inet_Pton
     (Af  : C.int;
      Cp  : C.Strings.chars_ptr;
      Inp : System.Address) return C.int;

   function C_Ioctl
     (Fd  : C.int;
      Req : C.int;
      Arg : access C.int) return C.int;

private
   pragma Import (C, Get_Socket_From_Set, "__gnat_get_socket_from_set");
   pragma Import (C, Is_Socket_In_Set, "__gnat_is_socket_in_set");
   pragma Import (C, Last_Socket_In_Set, "__gnat_last_socket_in_set");
   pragma Import (C, Insert_Socket_In_Set, "__gnat_insert_socket_in_set");
   pragma Import (C, Remove_Socket_From_Set, "__gnat_remove_socket_from_set");
   pragma Import (C, Reset_Socket_Set, "__gnat_reset_socket_set");
   pragma Import (C, C_Ioctl, "__gnat_socket_ioctl");
   pragma Import (C, Inet_Pton, SOSC.Inet_Pton_Linkname);

   pragma Import (C, Servent_S_Name, "__gnat_servent_s_name");
   pragma Import (C, Servent_S_Aliases, "__gnat_servent_s_aliases");
   pragma Import (C, Servent_S_Port, "__gnat_servent_s_port");
   pragma Import (C, Servent_S_Proto, "__gnat_servent_s_proto");
end GNAT.Sockets.Thin_Common;
