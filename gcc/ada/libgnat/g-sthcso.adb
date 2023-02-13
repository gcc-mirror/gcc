------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     G N A T . S O C K E T S . T H I N . C _ S O C K E T P A I R          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2023, AdaCore                     --
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

--  Portable sockets-based implementation of the C_Socketpair used for
--  platforms that do not support UNIX socketpair system call.

--  Note: this code is only for non-UNIX platforms.

separate (GNAT.Sockets.Thin)
function C_Socketpair
  (Domain   : C.int;
   Typ      : C.int;
   Protocol : C.int;
   Fds      : not null access Fd_Pair) return C.int
is
   --  This use type clause is not required on all platforms
   --  using this implementation. So we suppress the warning
   --  for the platforms that already use this type.
   pragma Warnings (Off, "use clause for type *");
   use type C.char_array;
   pragma Warnings (On, "use clause for type *");

   L_Sock, C_Sock, P_Sock : C.int := Failure;
   --  Listening socket, client socket and peer socket

   Family : constant Family_Type :=
              (case Domain is
                  when SOSC.AF_INET  => Family_Inet,
                  when SOSC.AF_INET6 => Family_Inet6,
                  when others        => Family_Unspec);

   Len   : aliased C.int := C.int (Lengths (Family));

   C_Sin : aliased Sockaddr;
   C_Bin : aliased C.char_array (1 .. C.size_t (Len));
   for C_Bin'Address use C_Sin'Address;
   --  Address of listening and client socket and it's binary representation.
   --  We need binary representation because Ada does not allow to compare
   --  unchecked union if either of the operands lacks inferable discriminants.
   --  RM-B-3-3 23/2.

   P_Sin : aliased Sockaddr;
   P_Bin : aliased C.char_array (1 .. C.size_t (Len));
   for P_Bin'Address use P_Sin'Address;
   --  Address of peer socket and it's binary representation

   T_Sin : aliased Sockaddr;
   T_Bin : aliased C.char_array (1 .. C.size_t (Len));
   for T_Bin'Address use T_Sin'Address;
   --  Temporary address to compare and check that address and port of the
   --  socket equal to peer address and port of the opposite connected socket.

   Res : C.int with Warnings => Off;

begin
   Set_Family (C_Sin.Sin_Family, Family);

   case Family is
      when Family_Inet =>
         C_Sin.Sin_Addr.S_B1 := 127;
         C_Sin.Sin_Addr.S_B4 := 1;

      when Family_Inet6 =>
         C_Sin.Sin6_Addr (C_Sin.Sin6_Addr'Last) := 1;

      when others =>
         Set_Socket_Errno (SOSC.EAFNOSUPPORT);
         return Failure;
   end case;

   for J in 1 .. 10 loop
      --  Retry loop, in case the C_Connect below fails

      C_Sin.Sin_Port := 0;

      --  Create a listening socket

      L_Sock := C_Socket (Domain, Typ, Protocol);
      exit when L_Sock = Failure;

      --  Bind the socket to an available port on localhost

      Res := C_Bind (L_Sock, C_Sin'Address, Len);
      exit when Res = Failure;

      --  Get assigned port

      Res := C_Getsockname (L_Sock, C_Sin'Address, Len'Access);
      exit when Res = Failure;

      --  Set socket to listen mode, with a backlog of 1 to guarantee that
      --  exactly one call to connect(2) succeeds.

      Res := C_Listen (L_Sock, 1);
      exit when Res = Failure;

      --  Create read end (client) socket

      C_Sock := C_Socket (Domain, Typ, Protocol);
      exit when C_Sock = Failure;

      --  Connect listening socket

      Res := C_Connect (C_Sock, C_Sin'Address, Len);

      if Res = Failure then
         --  In rare cases, the above C_Bind chooses a port that is still
         --  marked "in use", even though it has been closed (perhaps by some
         --  other process that has already exited). This causes the above
         --  C_Connect to fail with EADDRINUSE. In this case, we close the
         --  ports, and loop back to try again. This mysterious Windows
         --  behavior is documented. See, for example:
         --    http://msdn2.microsoft.com/en-us/library/ms737625.aspx
         --  In an experiment with 2000 calls, 21 required exactly one retry, 7
         --  required two, and none required three or more. Note that no delay
         --  is needed between retries; retrying C_Bind will typically produce
         --  a different port.

         exit when Socket_Errno /= SOSC.EADDRINUSE;

         goto Repeat;
      end if;

      --  Since the call to connect(2) has succeeded and the backlog limit
      --  on the listening socket is 1, we know that there is now exactly
      --  one pending connection on L_Sock, which is the one from R_Sock.

      P_Sin.Sun_Path := (others => C.nul);

      P_Sock := C_Accept (L_Sock, P_Sin'Address, Len'Access);
      exit when P_Sock = Failure;

      --  Address and port of the socket equal to peer address and port of the
      --  opposite connected socket.

      Res := C_Getsockname (P_Sock, T_Sin'Address, Len'Access);
      exit when Res = Failure;

      if T_Bin /= C_Bin then
         goto Repeat;
      end if;

      --  Address and port of the socket equal to peer address and port of the
      --  opposite connected socket.

      Res := C_Getsockname (C_Sock, T_Sin'Address, Len'Access);
      exit when Res = Failure;

      if T_Bin /= P_Bin then
         goto Repeat;
      end if;

      --  Close listening socket (ignore exit status)

      Res := C_Close (L_Sock);

      Fds.all := (Read_End => C_Sock, Write_End => P_Sock);

      return Thin_Common.Success;

      <<Repeat>>
      Res := C_Close (C_Sock);
      C_Sock := Failure;
      Res := C_Close (P_Sock);
      P_Sock := Failure;
      Res := C_Close (L_Sock);
      L_Sock := Failure;
   end loop;

   declare
      Saved_Errno : constant Integer := Socket_Errno;

   begin
      if P_Sock /= Failure then
         Res := C_Close (P_Sock);
      end if;

      if C_Sock /= Failure then
         Res := C_Close (C_Sock);
      end if;

      if L_Sock /= Failure then
         Res := C_Close (L_Sock);
      end if;

      Set_Socket_Errno (Saved_Errno);
   end;

   return Failure;
end C_Socketpair;
