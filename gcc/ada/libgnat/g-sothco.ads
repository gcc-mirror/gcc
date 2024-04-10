------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G N A T . S O C K E T S . T H I N _ C O M M O N             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2008-2024, AdaCore                     --
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

--  This is the target-independent part of the thin sockets mapping.
--  This package should not be directly with'ed by an applications program.

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System.Parameters;

package GNAT.Sockets.Thin_Common is

   package C renames Interfaces.C;
   package CS renames C.Strings;

   Success : constant C.int :=  0;
   Failure : constant C.int := -1;

   type time_t is
     range -2 ** (System.Parameters.time_t_bits - 1)
        .. 2 ** (System.Parameters.time_t_bits - 1) - 1;
   for time_t'Size use System.Parameters.time_t_bits;
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

   type socklen_t is mod 2 ** (8 * SOSC.SIZEOF_socklen_t);
   for socklen_t'Size use (8 * SOSC.SIZEOF_socklen_t);

   Immediat : constant Timeval := (0, 0);

   -------------------------------------------
   -- Mapping tables to low level constants --
   -------------------------------------------

   Families : constant array (Family_Type) of C.int :=
                [Family_Unspec => SOSC.AF_UNSPEC,
                 Family_Unix   => SOSC.AF_UNIX,
                 Family_Inet   => SOSC.AF_INET,
                 Family_Inet6  => SOSC.AF_INET6];

   Lengths  : constant array (Family_Type) of C.unsigned_char :=
                [Family_Unspec => 0,
                 Family_Unix   => SOSC.SIZEOF_sockaddr_un,
                 Family_Inet   => SOSC.SIZEOF_sockaddr_in,
                 Family_Inet6  => SOSC.SIZEOF_sockaddr_in6];

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
   end record with Unchecked_Union, Convention => C;

   procedure Set_Family
     (Length_And_Family : out Sockaddr_Length_And_Family;
      Family            : Family_Type);
   --  Set the family component to the appropriate value for Family, and also
   --  set Length accordingly if applicable on this platform.

   ----------------------------
   -- AF_INET socket address --
   ----------------------------

   type In_Addr is record
      S_B1, S_B2, S_B3, S_B4 : C.unsigned_char;
   end record with Convention => C, Alignment => C.int'Alignment;
   --  IPv4 address, represented as a network-order C.int. Note that the
   --  underlying operating system may assume that values of this type have
   --  C.int alignment, so we need to provide a suitable alignment clause here.

   function To_In_Addr is new Ada.Unchecked_Conversion (C.int, In_Addr);
   function To_Int     is new Ada.Unchecked_Conversion (In_Addr, C.int);

   function To_In_Addr (Addr : Inet_Addr_Type) return In_Addr;
   procedure To_Inet_Addr
     (Addr   : In_Addr;
      Result : out Inet_Addr_Type);
   --  Conversion functions

   type In6_Addr is array (1 .. 16) of C.unsigned_char with Convention => C;

   Unix_Name_Length : constant := 108;
   --  Maximum length for local unix socket name

   function To_In6_Addr (Addr : Inet_Addr_Type) return In6_Addr;
   procedure To_Inet_Addr
     (Addr   : In6_Addr;
      Result : out Inet_Addr_Type);
   --  Conversion functions

   type Sockaddr (Family : Family_Type := Family_Inet) is record
      case Family is
      when Family_Inet =>
         Sin_Family : Sockaddr_Length_And_Family;
         --  Address family (and address length on some platforms)

         Sin_Port : C.unsigned_short;
         --  Port in network byte order

         Sin_Addr : In_Addr := (others => 0);
         --  IPv4 address

         Sin_Zero : C.char_array (1 .. 8) := [others => C.nul];
         --  Padding
         --
         --  Note that some platforms require that all unused (reserved) bytes
         --  in addresses be initialized to 0 (e.g. VxWorks).

      when Family_Inet6 =>
         Sin6_Family : Sockaddr_Length_And_Family;
         --  Address family (and address length on some platforms)

         Sin6_Port : C.unsigned_short;
         --  Port in network byte order

         Sin6_FlowInfo : Interfaces.Unsigned_32 := 0;
         Sin6_Addr     : In6_Addr := [others => 0];
         Sin6_Scope_Id : Interfaces.Unsigned_32 := 0;

      when Family_Unix =>
         Sun_Family : Sockaddr_Length_And_Family;
         --  Address family (and address length on some platforms)

         Sun_Path : C.char_array (1 .. Unix_Name_Length);

      when Family_Unspec =>
         null;
      end case;
   end record with Convention => C, Unchecked_Union;
   --  Internet socket address

   type Sockaddr_Access is access all Sockaddr;
   pragma Convention (C, Sockaddr_Access);
   --  Access to internet socket address

   procedure Set_Address
     (Sin     : Sockaddr_Access;
      Address : Sock_Addr_Type;
      Length  : out C.int);
   --  Initialise all necessary fields in Sin from Address.
   --  Set appropriate Family, Port, and either Sin.Sin_Addr or Sin.Sin6_Addr
   --  depend on family.
   --  Set the Length out parameter to the valuable Sockaddr data length.

   function Get_Address (Sin : Sockaddr; Length : C.int) return Sock_Addr_Type;
   --  Get Sock_Addr_Type from Sockaddr and its valuable data Length

   ------------------
   -- Host entries --
   ------------------

   type Hostent is new
     System.Storage_Elements.Storage_Array (1 .. SOSC.SIZEOF_struct_hostent);
   for Hostent'Alignment use 8;
   --  Host entry. This is an opaque type used only via the following
   --  accessor functions, because 'struct hostent' has different layouts on
   --  different platforms.

   type Hostent_Access is access all Hostent;
   pragma Convention (C, Hostent_Access);
   --  Access to host entry

   function Hostent_H_Name
     (E : Hostent_Access) return System.Address;

   function Hostent_H_Alias
     (E : Hostent_Access; I : C.int) return System.Address;

   function Hostent_H_Addrtype
     (E : Hostent_Access) return C.int;

   function Hostent_H_Length
     (E : Hostent_Access) return C.int;

   function Hostent_H_Addr
     (E : Hostent_Access; Index : C.int) return System.Address;

   ---------------------
   -- Service entries --
   ---------------------

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
     (E : Servent_Access) return System.Address;

   function Servent_S_Alias
     (E : Servent_Access; Index : C.int) return System.Address;

   function Servent_S_Port
     (E : Servent_Access) return C.unsigned_short;

   function Servent_S_Proto
     (E : Servent_Access) return System.Address;

   ------------------
   -- NetDB access --
   ------------------

   --  There are three possible situations for the following NetDB access
   --  functions:
   --    - inherently thread safe (case of data returned in a thread specific
   --      buffer);
   --    - thread safe using user-provided buffer;
   --    - thread unsafe.
   --
   --  In the first and third cases, the Buf and Buflen are ignored. In the
   --  second case, the caller must provide a buffer large enough to
   --  accommodate the returned data. In the third case, the caller must ensure
   --  that these functions are called within a critical section.

   function C_Gethostbyname
     (Name     : C.char_array;
      Ret      : not null access Hostent;
      Buf      : System.Address;
      Buflen   : C.size_t;
      H_Errnop : not null access C.int) return C.int;

   function C_Gethostbyaddr
     (Addr      : System.Address;
      Addr_Len  : C.int;
      Addr_Type : C.int;
      Ret       : not null access Hostent;
      Buf       : System.Address;
      Buflen    : C.size_t;
      H_Errnop  : not null access C.int) return C.int;

   function C_Getservbyname
     (Name   : C.char_array;
      Proto  : C.char_array;
      Ret    : not null access Servent;
      Buf    : System.Address;
      Buflen : C.size_t) return C.int;

   function C_Getservbyport
     (Port   : C.int;
      Proto  : C.char_array;
      Ret    : not null access Servent;
      Buf    : System.Address;
      Buflen : C.size_t) return C.int;

   Address_Size : constant := Standard'Address_Size;

   type Addrinfo;
   type Addrinfo_Access is access all Addrinfo;

   type Addrinfo is record
      ai_flags     : C.int;
      ai_family    : C.int;
      ai_socktype  : C.int;
      ai_protocol  : C.int;
      ai_addrlen   : socklen_t;
      ai_addr      : Sockaddr_Access;
      ai_canonname : CS.char_array_access;
      ai_next      : Addrinfo_Access;
   end record with Convention => C;
   for Addrinfo use record
      ai_flags     at SOSC.AI_FLAGS_OFFSET     range 0 .. C.int'Size - 1;
      ai_family    at SOSC.AI_FAMILY_OFFSET    range 0 .. C.int'Size - 1;
      ai_socktype  at SOSC.AI_SOCKTYPE_OFFSET  range 0 .. C.int'Size - 1;
      ai_protocol  at SOSC.AI_PROTOCOL_OFFSET  range 0 .. C.int'Size - 1;
      ai_addrlen   at SOSC.AI_ADDRLEN_OFFSET   range 0 .. socklen_t'Size - 1;
      ai_canonname at SOSC.AI_CANONNAME_OFFSET range 0 .. Address_Size - 1;
      ai_addr      at SOSC.AI_ADDR_OFFSET      range 0 .. Address_Size - 1;
      ai_next      at SOSC.AI_NEXT_OFFSET      range 0 .. Address_Size - 1;
   end record;

   function C_Getaddrinfo
     (Node    : CS.char_array_access;
      Service : CS.char_array_access;
      Hints   : access constant Addrinfo;
      Res     : not null access Addrinfo_Access) return C.int;

   procedure C_Freeaddrinfo (res : Addrinfo_Access);

   function C_Getnameinfo
     (sa      : Sockaddr_Access;
      salen   : socklen_t;
      host    : CS.char_array_access;
      hostlen : C.size_t;
      serv    : CS.char_array_access;
      servlen : C.size_t;
      flags   : C.int) return C.int;

   function C_GAI_Strerror (ecode : C.int) return CS.chars_ptr;

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
   --  Indexes into an Fd_Pair value providing access to each of the connected
   --  file descriptors.

   function Inet_Pton
     (Af  : C.int;
      Cp  : System.Address;
      Inp : System.Address) return C.int;

   function Inet_Ntop
     (Af   : C.int;
      Src  : System.Address;
      Dst  : CS.char_array_access;
      Size : socklen_t) return CS.char_array_access;

   function C_Ioctl
     (Fd  : C.int;
      Req : SOSC.IOCTL_Req_T;
      Arg : access C.int) return C.int;

   function Short_To_Network
     (S : C.unsigned_short) return C.unsigned_short;
   pragma Inline (Short_To_Network);
   --  Convert a port number into a network port number

   function Network_To_Short
     (S : C.unsigned_short) return C.unsigned_short
   renames Short_To_Network;
   --  Symmetric operation

   Minus_500ms_Windows_Timeout : constant Boolean;
   --  Microsoft Windows desktop older then 8.0 and Microsoft Windows Server
   --  older than 2019 need timeout correction for 500 milliseconds. This
   --  constant is True for such versions.

private

   function Get_Minus_500ms_Timeout return C.int
     with Import, Convention => C, External_Name => "__gnat_minus_500ms";

   Minus_500ms_Windows_Timeout : constant Boolean :=
                                   Get_Minus_500ms_Timeout /= 0;

   pragma Import (C, Get_Socket_From_Set, "__gnat_get_socket_from_set");
   pragma Import (C, Is_Socket_In_Set, "__gnat_is_socket_in_set");
   pragma Import (C, Last_Socket_In_Set, "__gnat_last_socket_in_set");
   pragma Import (C, Insert_Socket_In_Set, "__gnat_insert_socket_in_set");
   pragma Import (C, Remove_Socket_From_Set, "__gnat_remove_socket_from_set");
   pragma Import (C, Reset_Socket_Set, "__gnat_reset_socket_set");
   pragma Import (C, C_Ioctl, "__gnat_socket_ioctl");
   pragma Import (C, Inet_Pton, SOSC.Inet_Pton_Linkname);
   pragma Import (C, Inet_Ntop, SOSC.Inet_Ntop_Linkname);

   pragma Import (C, C_Gethostbyname, "__gnat_gethostbyname");
   pragma Import (C, C_Gethostbyaddr, "__gnat_gethostbyaddr");
   pragma Import (C, C_Getservbyname, "__gnat_getservbyname");
   pragma Import (C, C_Getservbyport, "__gnat_getservbyport");

   pragma Import (C, C_Getaddrinfo,   "__gnat_getaddrinfo");
   pragma Import (C, C_Freeaddrinfo,  "__gnat_freeaddrinfo");
   pragma Import (C, C_Getnameinfo,   "__gnat_getnameinfo");
   pragma Import (C, C_GAI_Strerror,  "__gnat_gai_strerror");

   pragma Import (C, Servent_S_Name,  "__gnat_servent_s_name");
   pragma Import (C, Servent_S_Alias, "__gnat_servent_s_alias");
   pragma Import (C, Servent_S_Port,  "__gnat_servent_s_port");
   pragma Import (C, Servent_S_Proto, "__gnat_servent_s_proto");

   pragma Import (C, Hostent_H_Name,     "__gnat_hostent_h_name");
   pragma Import (C, Hostent_H_Alias,    "__gnat_hostent_h_alias");
   pragma Import (C, Hostent_H_Addrtype, "__gnat_hostent_h_addrtype");
   pragma Import (C, Hostent_H_Length,   "__gnat_hostent_h_length");
   pragma Import (C, Hostent_H_Addr,     "__gnat_hostent_h_addr");

end GNAT.Sockets.Thin_Common;
