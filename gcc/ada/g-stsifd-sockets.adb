------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     G N A T . S O C K E T S . T H I N . S I G N A L L I N G _ F D S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2010, AdaCore                     --
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

--  Portable sockets-based implementation of GNAT.Sockets.Thin.Signalling_Fds
--  used for platforms that do not support UNIX pipes.

--  Note: this code used to be in GNAT.Sockets, but has been moved to a
--  platform-specific file. It is now used only for non-UNIX platforms.

separate (GNAT.Sockets.Thin)
package body Signalling_Fds is

   -----------
   -- Close --
   -----------

   procedure Close (Sig : C.int) is
      Res : C.int;
      pragma Unreferenced (Res);
      --  Res is assigned but never read, because we purposefully ignore
      --  any error returned by the C_Close system call, as per the spec
      --  of this procedure.
   begin
      Res := C_Close (Sig);
   end Close;

   ------------
   -- Create --
   ------------

   function Create (Fds : not null access Fd_Pair) return C.int is
      L_Sock, R_Sock, W_Sock : C.int := Failure;
      --  Listening socket, read socket and write socket

      Sin : aliased Sockaddr_In;
      Len : aliased C.int;
      --  Address of listening socket

      Res : C.int;
      pragma Warnings (Off, Res);
      --  Return status of system calls (usually ignored, hence warnings off)

   begin
      Fds.all := (Read_End | Write_End => Failure);

      --  We open two signalling sockets. One of them is used to send data
      --  to the other, which is included in a C_Select socket set. The
      --  communication is used to force the call to C_Select to complete,
      --  and the waiting task to resume its execution.

      loop
         --  Retry loop, in case the C_Connect below fails

         --  Create a listening socket

         L_Sock := C_Socket (SOSC.AF_INET, SOSC.SOCK_STREAM, 0);

         if L_Sock = Failure then
            goto Fail;
         end if;

         --  Bind the socket to an available port on localhost

         Set_Family (Sin.Sin_Family, Family_Inet);
         Sin.Sin_Addr.S_B1 := 127;
         Sin.Sin_Addr.S_B2 := 0;
         Sin.Sin_Addr.S_B3 := 0;
         Sin.Sin_Addr.S_B4 := 1;
         Sin.Sin_Port      := 0;

         Len := C.int (Lengths (Family_Inet));
         Res := C_Bind (L_Sock, Sin'Address, Len);

         if Res = Failure then
            goto Fail;
         end if;

         --  Get assigned port

         Res := C_Getsockname (L_Sock, Sin'Address, Len'Access);
         if Res = Failure then
            goto Fail;
         end if;

         --  Set socket to listen mode, with a backlog of 1 to guarantee that
         --  exactly one call to connect(2) succeeds.

         Res := C_Listen (L_Sock, 1);

         if Res = Failure then
            goto Fail;
         end if;

         --  Create read end (client) socket

         R_Sock := C_Socket (SOSC.AF_INET, SOSC.SOCK_STREAM, 0);

         if R_Sock = Failure then
            goto Fail;
         end if;

         --  Connect listening socket

         Res := C_Connect (R_Sock, Sin'Address, Len);

         exit when Res /= Failure;

         if Socket_Errno /= SOSC.EADDRINUSE then
            goto Fail;
         end if;

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

         pragma Assert (Res = Failure
                          and then
                        Socket_Errno = SOSC.EADDRINUSE);
         Res := C_Close (W_Sock);
         W_Sock := Failure;
         Res := C_Close (R_Sock);
         R_Sock := Failure;
      end loop;

      --  Since the call to connect(2) has succeeded and the backlog limit on
      --  the listening socket is 1, we know that there is now exactly one
      --  pending connection on L_Sock, which is the one from R_Sock.

      W_Sock := C_Accept (L_Sock, Sin'Address, Len'Access);

      if W_Sock = Failure then
         goto Fail;
      end if;

      --  Set TCP_NODELAY on W_Sock, since we always want to send the data out
      --  immediately.

      Set_Socket_Option
        (Socket => Socket_Type (W_Sock),
         Level  => IP_Protocol_For_TCP_Level,
         Option => (Name => No_Delay, Enabled => True));

      --  Close listening socket (ignore exit status)

      Res := C_Close (L_Sock);

      Fds.all := (Read_End => R_Sock, Write_End => W_Sock);

      return Thin_Common.Success;

   <<Fail>>
      declare
         Saved_Errno : constant Integer := Socket_Errno;

      begin
         if W_Sock /= Failure then
            Res := C_Close (W_Sock);
         end if;

         if R_Sock /= Failure then
            Res := C_Close (R_Sock);
         end if;

         if L_Sock /= Failure then
            Res := C_Close (L_Sock);
         end if;

         Set_Socket_Errno (Saved_Errno);
      end;

      return Failure;
   end Create;

   ----------
   -- Read --
   ----------

   function Read (Rsig : C.int) return C.int is
      Buf : aliased Character;
   begin
      return C_Recv (Rsig, Buf'Address, 1, SOSC.MSG_Forced_Flags);
   end Read;

   -----------
   -- Write --
   -----------

   function Write (Wsig : C.int) return C.int is
      Buf : aliased Character := ASCII.NUL;
   begin
      return C_Sendto
        (Wsig, Buf'Address, 1,
         Flags => SOSC.MSG_Forced_Flags,
         To    => System.Null_Address,
         Tolen => 0);
   end Write;

end Signalling_Fds;
