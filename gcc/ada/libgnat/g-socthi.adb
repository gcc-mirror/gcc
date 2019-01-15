------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This package provides a target dependent thin interface to the sockets
--  layer for use by the GNAT.Sockets package (g-socket.ads). This package
--  should not be directly with'ed by an applications program.

--  This is the default version

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Task_Lock;

with Interfaces.C; use Interfaces.C;

package body GNAT.Sockets.Thin is

   Non_Blocking_Sockets : aliased Fd_Set;
   --  When this package is initialized with Process_Blocking_IO set
   --  to True, sockets are set in non-blocking mode to avoid blocking
   --  the whole process when a thread wants to perform a blocking IO
   --  operation. But the user can also set a socket in non-blocking
   --  mode by purpose. In order to make a difference between these
   --  two situations, we track the origin of non-blocking mode in
   --  Non_Blocking_Sockets. If S is in Non_Blocking_Sockets, it has
   --  been set in non-blocking mode by the user.

   Quantum : constant Duration := 0.2;
   --  When SOSC.Thread_Blocking_IO is False, we set sockets in
   --  non-blocking mode and we spend a period of time Quantum between
   --  two attempts on a blocking operation.

   --  Comments required for following functions ???

   function Syscall_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : not null access C.int) return C.int;
   pragma Import (C, Syscall_Accept, "accept");

   function Syscall_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int;
   pragma Import (C, Syscall_Connect, "connect");

   function Syscall_Recv
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int) return C.int;
   pragma Import (C, Syscall_Recv, "recv");

   function Syscall_Recvfrom
     (S       : C.int;
      Msg     : System.Address;
      Len     : C.int;
      Flags   : C.int;
      From    : System.Address;
      Fromlen : not null access C.int) return C.int;
   pragma Import (C, Syscall_Recvfrom, "recvfrom");

   function Syscall_Recvmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t;
   pragma Import (C, Syscall_Recvmsg, "recvmsg");

   function Syscall_Sendmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t;
   pragma Import (C, Syscall_Sendmsg, "sendmsg");

   function Syscall_Sendto
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int;
      To    : System.Address;
      Tolen : C.int) return C.int;
   pragma Import (C, Syscall_Sendto, "sendto");

   function Syscall_Socket
     (Domain   : C.int;
      Typ      : C.int;
      Protocol : C.int) return C.int;
   pragma Import (C, Syscall_Socket, "socket");

   procedure Disable_SIGPIPE (S : C.int);
   pragma Import (C, Disable_SIGPIPE, "__gnat_disable_sigpipe");

   procedure Disable_All_SIGPIPEs;
   pragma Import (C, Disable_All_SIGPIPEs, "__gnat_disable_all_sigpipes");
   --  Sets the process to ignore all SIGPIPE signals on platforms that
   --  don't support Disable_SIGPIPE for particular streams.

   function Non_Blocking_Socket (S : C.int) return Boolean;
   procedure Set_Non_Blocking_Socket (S : C.int; V : Boolean);

   --------------
   -- C_Accept --
   --------------

   function C_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : not null access C.int) return C.int
   is
      R   : C.int;
      Val : aliased C.int := 1;

      Discard : C.int;
      pragma Warnings (Off, Discard);

   begin
      loop
         R := Syscall_Accept (S, Addr, Addrlen);
         exit when SOSC.Thread_Blocking_IO
           or else R /= Failure
           or else Non_Blocking_Socket (S)
           or else Errno /= SOSC.EWOULDBLOCK;
         delay Quantum;
      end loop;

      if not SOSC.Thread_Blocking_IO
        and then R /= Failure
      then
         --  A socket inherits the properties ot its server especially
         --  the FIONBIO flag. Do not use Socket_Ioctl as this subprogram
         --  tracks sockets set in non-blocking mode by user.

         Set_Non_Blocking_Socket (R, Non_Blocking_Socket (S));
         Discard := C_Ioctl (R, SOSC.FIONBIO, Val'Access);
      end if;

      Disable_SIGPIPE (R);
      return R;
   end C_Accept;

   ---------------
   -- C_Connect --
   ---------------

   function C_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int
   is
      Res : C.int;

   begin
      Res := Syscall_Connect (S, Name, Namelen);

      if SOSC.Thread_Blocking_IO
        or else Res /= Failure
        or else Non_Blocking_Socket (S)
        or else Errno /= SOSC.EINPROGRESS
      then
         return Res;
      end if;

      declare
         WSet : aliased Fd_Set;
         Now  : aliased Timeval;

      begin
         Reset_Socket_Set (WSet'Access);
         loop
            Insert_Socket_In_Set (WSet'Access, S);
            Now := Immediat;
            Res := C_Select
              (S + 1,
               No_Fd_Set_Access,
               WSet'Access,
               No_Fd_Set_Access,
               Now'Unchecked_Access);

            exit when Res > 0;

            if Res = Failure then
               return Res;
            end if;

            delay Quantum;
         end loop;
      end;

      Res := Syscall_Connect (S, Name, Namelen);

      if Res = Failure
        and then Errno = SOSC.EISCONN
      then
         return Thin_Common.Success;
      else
         return Res;
      end if;
   end C_Connect;

   ------------------
   -- Socket_Ioctl --
   ------------------

   function Socket_Ioctl
     (S   : C.int;
      Req : SOSC.IOCTL_Req_T;
      Arg : access C.int) return C.int
   is
   begin
      if not SOSC.Thread_Blocking_IO and then Req = SOSC.FIONBIO then
         if Arg.all /= 0 then
            Set_Non_Blocking_Socket (S, True);
         end if;
      end if;

      return C_Ioctl (S, Req, Arg);
   end Socket_Ioctl;

   ------------
   -- C_Recv --
   ------------

   function C_Recv
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Recv (S, Msg, Len, Flags);
         exit when SOSC.Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
           or else Errno /= SOSC.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Recv;

   ----------------
   -- C_Recvfrom --
   ----------------

   function C_Recvfrom
     (S       : C.int;
      Msg     : System.Address;
      Len     : C.int;
      Flags   : C.int;
      From    : System.Address;
      Fromlen : not null access C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Recvfrom (S, Msg, Len, Flags, From, Fromlen);
         exit when SOSC.Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
           or else Errno /= SOSC.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Recvfrom;

   ---------------
   -- C_Recvmsg --
   ---------------

   function C_Recvmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t
   is
      Res : System.CRTL.ssize_t;

   begin
      loop
         Res := Syscall_Recvmsg (S, Msg, Flags);
         exit when SOSC.Thread_Blocking_IO
           or else Res /= System.CRTL.ssize_t (Failure)
           or else Non_Blocking_Socket (S)
           or else Errno /= SOSC.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Recvmsg;

   ---------------
   -- C_Sendmsg --
   ---------------

   function C_Sendmsg
     (S     : C.int;
      Msg   : System.Address;
      Flags : C.int) return System.CRTL.ssize_t
   is
      Res : System.CRTL.ssize_t;

   begin
      loop
         Res := Syscall_Sendmsg (S, Msg, Flags);
         exit when SOSC.Thread_Blocking_IO
           or else Res /= System.CRTL.ssize_t (Failure)
           or else Non_Blocking_Socket (S)
           or else Errno /= SOSC.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Sendmsg;

   --------------
   -- C_Sendto --
   --------------

   function C_Sendto
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int;
      To    : System.Address;
      Tolen : C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Sendto (S, Msg, Len, Flags, To, Tolen);
         exit when SOSC.Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
           or else Errno /= SOSC.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Sendto;

   --------------
   -- C_Socket --
   --------------

   function C_Socket
     (Domain   : C.int;
      Typ      : C.int;
      Protocol : C.int) return C.int
   is
      R   : C.int;
      Val : aliased C.int := 1;

      Discard : C.int;

   begin
      R := Syscall_Socket (Domain, Typ, Protocol);

      if not SOSC.Thread_Blocking_IO
        and then R /= Failure
      then
         --  Do not use Socket_Ioctl as this subprogram tracks sockets set
         --  in non-blocking mode by user.

         Discard := C_Ioctl (R, SOSC.FIONBIO, Val'Access);
         Set_Non_Blocking_Socket (R, False);
      end if;
      Disable_SIGPIPE (R);
      return R;
   end C_Socket;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   -------------------------
   -- Host_Error_Messages --
   -------------------------

   package body Host_Error_Messages is separate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Disable_All_SIGPIPEs;
      Reset_Socket_Set (Non_Blocking_Sockets'Access);
   end Initialize;

   -------------------------
   -- Non_Blocking_Socket --
   -------------------------

   function Non_Blocking_Socket (S : C.int) return Boolean is
      R : Boolean;
   begin
      Task_Lock.Lock;
      R := (Is_Socket_In_Set (Non_Blocking_Sockets'Access, S) /= 0);
      Task_Lock.Unlock;
      return R;
   end Non_Blocking_Socket;

   -----------------------------
   -- Set_Non_Blocking_Socket --
   -----------------------------

   procedure Set_Non_Blocking_Socket (S : C.int; V : Boolean) is
   begin
      Task_Lock.Lock;

      if V then
         Insert_Socket_In_Set (Non_Blocking_Sockets'Access, S);
      else
         Remove_Socket_From_Set (Non_Blocking_Sockets'Access, S);
      end if;

      Task_Lock.Unlock;
   end Set_Non_Blocking_Socket;

   --------------------
   -- Signalling_Fds --
   --------------------

   package body Signalling_Fds is

      --  In this default implementation, we use a C version of these
      --  subprograms provided by socket.c.

      function C_Create (Fds : not null access Fd_Pair) return C.int;
      function C_Read (Rsig : C.int) return C.int;
      function C_Write (Wsig : C.int) return C.int;
      procedure C_Close (Sig : C.int);

      pragma Import (C, C_Create, "__gnat_create_signalling_fds");
      pragma Import (C, C_Read,   "__gnat_read_signalling_fd");
      pragma Import (C, C_Write,  "__gnat_write_signalling_fd");
      pragma Import (C, C_Close,  "__gnat_close_signalling_fd");

      function Create
        (Fds : not null access Fd_Pair) return C.int renames C_Create;
      function Read (Rsig : C.int) return C.int renames C_Read;
      function Write (Wsig : C.int) return C.int renames C_Write;
      procedure Close (Sig : C.int) renames C_Close;

   end Signalling_Fds;

   --------------------------
   -- Socket_Error_Message --
   --------------------------

   function Socket_Error_Message (Errno : Integer) return String is separate;

end GNAT.Sockets.Thin;
