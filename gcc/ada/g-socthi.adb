------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--              Copyright (C) 2001 Ada Core Technologies, Inc.              --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Interfaces.C; use Interfaces.C;

package body GNAT.Sockets.Thin is

   --  When this package is initialized with Process_Blocking_IO set
   --  to True, sockets are set in non-blocking mode to avoid blocking
   --  the whole process when a thread wants to perform a blocking IO
   --  operation. But the user can set a socket in non-blocking mode
   --  by purpose. We track the socket in such a mode by redefining
   --  C_Ioctl. In blocking IO operations, we exit normally when the
   --  non-blocking flag is set by user, we poll and try later when
   --  this flag is set automatically by this package.

   type Socket_Info is record
      Non_Blocking : Boolean := False;
   end record;

   Table : array (C.int range 0 .. 31) of Socket_Info;
   --  Get info on blocking flag. This array is limited to 32 sockets
   --  because the select operation allows socket set of less then 32
   --  sockets.

   Quantum : constant Duration := 0.2;
   --  comment needed ???

   Thread_Blocking_IO : Boolean := True;

   function Syscall_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : access C.int)
      return    C.int;
   pragma Import (C, Syscall_Accept, "accept");

   function Syscall_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int)
      return    C.int;
   pragma Import (C, Syscall_Connect, "connect");

   function Syscall_Ioctl
     (S    : C.int;
      Req  : C.int;
      Arg  : Int_Access)
      return C.int;
   pragma Import (C, Syscall_Ioctl, "ioctl");

   function Syscall_Recv
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int)
      return  C.int;
   pragma Import (C, Syscall_Recv, "recv");

   function Syscall_Recvfrom
     (S       : C.int;
      Msg     : System.Address;
      Len     : C.int;
      Flags   : C.int;
      From    : Sockaddr_In_Access;
      Fromlen : access C.int)
      return    C.int;
   pragma Import (C, Syscall_Recvfrom, "recvfrom");

   function Syscall_Send
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int)
      return  C.int;
   pragma Import (C, Syscall_Send, "send");

   function Syscall_Sendto
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int;
      To    : Sockaddr_In_Access;
      Tolen : C.int)
      return  C.int;
   pragma Import (C, Syscall_Sendto, "sendto");

   function Syscall_Socket
     (Domain, Typ, Protocol : C.int)
      return C.int;
   pragma Import (C, Syscall_Socket, "socket");

   procedure Set_Non_Blocking (S : C.int);

   --------------
   -- C_Accept --
   --------------

   function C_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : access C.int)
      return    C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Accept (S, Addr, Addrlen);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Table (S).Non_Blocking
           or else Errno /= Constants.EWOULDBLOCK;
         delay Quantum;
      end loop;

      if not Thread_Blocking_IO
        and then Res /= Failure
      then
         --  A socket inherits the properties ot its server especially
         --  the FNDELAY flag.

         Table (Res).Non_Blocking := Table (S).Non_Blocking;
         Set_Non_Blocking (Res);
      end if;

      return Res;
   end C_Accept;

   ---------------
   -- C_Connect --
   ---------------

   function C_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int)
      return    C.int
   is
      Res : C.int;

   begin
      Res := Syscall_Connect (S, Name, Namelen);

      if Thread_Blocking_IO
        or else Res /= Failure
        or else Table (S).Non_Blocking
        or else Errno /= Constants.EINPROGRESS
      then
         return Res;
      end if;

      declare
         Set : aliased Fd_Set;
         Now : aliased Timeval;

      begin
         loop
            Set := 2 ** Natural (S);
            Now := Immediat;
            Res := C_Select
              (S + 1,
               null, Set'Unchecked_Access,
               null, Now'Unchecked_Access);

            exit when Res > 0;

            if Res = Failure then
               return Res;
            end if;

            delay Quantum;
         end loop;
      end;

      Res := Syscall_Connect (S, Name, Namelen);

      if Res = Failure
        and then Errno = Constants.EISCONN
      then
         return Thin.Success;
      else
         return Res;
      end if;
   end C_Connect;

   -------------
   -- C_Ioctl --
   -------------

   function C_Ioctl
     (S    : C.int;
      Req  : C.int;
      Arg  : Int_Access)
      return C.int
   is
   begin
      if not Thread_Blocking_IO
        and then Req = Constants.FIONBIO
      then
         Table (S).Non_Blocking := (Arg.all /= 0);
      end if;

      return Syscall_Ioctl (S, Req, Arg);
   end C_Ioctl;

   ------------
   -- C_Recv --
   ------------

   function C_Recv
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int)
      return  C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Recv (S, Msg, Len, Flags);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Table (S).Non_Blocking
           or else Errno /= Constants.EWOULDBLOCK;
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
      From    : Sockaddr_In_Access;
      Fromlen : access C.int)
      return    C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Recvfrom (S, Msg, Len, Flags, From, Fromlen);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Table (S).Non_Blocking
           or else Errno /= Constants.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Recvfrom;

   ------------
   -- C_Send --
   ------------

   function C_Send
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int)
      return  C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Send (S, Msg, Len, Flags);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Table (S).Non_Blocking
           or else Errno /= Constants.EWOULDBLOCK;
         delay Quantum;
      end loop;

      return Res;
   end C_Send;

   --------------
   -- C_Sendto --
   --------------

   function C_Sendto
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int;
      To    : Sockaddr_In_Access;
      Tolen : C.int)
      return  C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Sendto (S, Msg, Len, Flags, To, Tolen);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Table (S).Non_Blocking
           or else Errno /= Constants.EWOULDBLOCK;
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
      Protocol : C.int)
      return     C.int
   is
      Res : C.int;

   begin
      Res := Syscall_Socket (Domain, Typ, Protocol);

      if not Thread_Blocking_IO
        and then Res /= Failure
      then
         Set_Non_Blocking (Res);
      end if;

      return Res;
   end C_Socket;

   -----------
   -- Clear --
   -----------

   procedure Clear
     (Item   : in out Fd_Set;
      Socket : in C.int)
   is
      Mask : constant Fd_Set := 2 ** Natural (Socket);

   begin
      if (Item and Mask) /= 0 then
         Item := Item xor Mask;
      end if;
   end Clear;

   -----------
   -- Empty --
   -----------

   procedure Empty  (Item : in out Fd_Set) is
   begin
      Item := 0;
   end Empty;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      null;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Process_Blocking_IO : Boolean) is
   begin
      Thread_Blocking_IO := not Process_Blocking_IO;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Item : Fd_Set) return Boolean is
   begin
      return Item = 0;
   end Is_Empty;

   ------------
   -- Is_Set --
   ------------

   function Is_Set (Item : Fd_Set; Socket : C.int) return Boolean is
   begin
      return (Item and 2 ** Natural (Socket)) /= 0;
   end Is_Set;

   ---------
   -- Max --
   ---------

   function Max (Item : Fd_Set) return C.int
   is
      L : C.int  := -1;
      C : Fd_Set := Item;

   begin
      while C /= 0 loop
         L := L + 1;
         C := C / 2;
      end loop;
      return L;
   end Max;

   ---------
   -- Set --
   ---------

   procedure Set (Item : in out Fd_Set; Socket : in C.int) is
   begin
      Item := Item or 2 ** Natural (Socket);
   end Set;

   ----------------------
   -- Set_Non_Blocking --
   ----------------------

   procedure Set_Non_Blocking (S : C.int) is
      Res : C.int;
      Val : aliased C.int := 1;

   begin

      --  Do not use C_Fcntl because this subprogram tracks the
      --  sockets set by user in non-blocking mode.

      Res := Syscall_Ioctl (S, Constants.FIONBIO, Val'Unchecked_Access);
   end Set_Non_Blocking;

   --------------------------
   -- Socket_Error_Message --
   --------------------------

   function Socket_Error_Message (Errno : Integer) return String is
      use type Interfaces.C.Strings.chars_ptr;

      C_Msg : C.Strings.chars_ptr;

   begin
      C_Msg := C_Strerror (C.int (Errno));

      if C_Msg = C.Strings.Null_Ptr then
         return "Unknown system error";

      else
         return C.Strings.Value (C_Msg);
      end if;
   end Socket_Error_Message;

end GNAT.Sockets.Thin;
