------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    G N A T . S O C K E T S . T H I N                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002-2004 Ada Core Technologies, Inc.           --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides a target dependent thin interface to the sockets
--  layer for use by the GNAT.Sockets package (g-socket.ads). This package
--  should not be directly with'ed by an applications program.

--  This version is for VxWorks

with GNAT.OS_Lib;  use GNAT.OS_Lib;
with GNAT.Task_Lock;

with Interfaces.C; use Interfaces.C;
with Unchecked_Conversion;

package body GNAT.Sockets.Thin is

   Non_Blocking_Sockets : constant Fd_Set_Access :=
                            New_Socket_Set (No_Socket_Set);
   --  When this package is initialized with Process_Blocking_IO set
   --  to True, sockets are set in non-blocking mode to avoid blocking
   --  the whole process when a thread wants to perform a blocking IO
   --  operation. But the user can also set a socket in non-blocking
   --  mode by purpose. In order to make a difference between these
   --  two situations, we track the origin of non-blocking mode in
   --  Non_Blocking_Sockets. If S is in Non_Blocking_Sockets, it has
   --  been set in non-blocking mode by the user.

   Quantum : constant Duration := 0.2;
   --  When Thread_Blocking_IO is False, we set sockets in
   --  non-blocking mode and we spend a period of time Quantum between
   --  two attempts on a blocking operation.

   Thread_Blocking_IO : Boolean := True;

   Unknown_System_Error : constant C.Strings.chars_ptr :=
                            C.Strings.New_String ("Unknown system error");

   --  The following types and variables are required to create a Hostent
   --  record "by hand".

   type In_Addr_Access_Array_Access is access In_Addr_Access_Array;

   Alias_Access : constant Chars_Ptr_Pointers.Pointer :=
                    new C.Strings.chars_ptr'(C.Strings.Null_Ptr);

   In_Addr_Access_Array_A : constant In_Addr_Access_Array_Access :=
                              new In_Addr_Access_Array'(new In_Addr, null);

   In_Addr_Access_Ptr : constant In_Addr_Access_Pointers.Pointer :=
                          In_Addr_Access_Array_A
                            (In_Addr_Access_Array_A'First)'Access;

   Local_Hostent : constant Hostent_Access := new Hostent;

   -----------------------
   -- Local Subprograms --
   -----------------------

   --  All these require comments ???

   function Syscall_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : access C.int) return C.int;
   pragma Import (C, Syscall_Accept, "accept");

   function Syscall_Connect
     (S       : C.int;
      Name    : System.Address;
      Namelen : C.int) return C.int;
   pragma Import (C, Syscall_Connect, "connect");

   function Syscall_Ioctl
     (S    : C.int;
      Req  : C.int;
      Arg  : Int_Access) return C.int;
   pragma Import (C, Syscall_Ioctl, "ioctl");

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
      From    : Sockaddr_In_Access;
      Fromlen : access C.int) return C.int;
   pragma Import (C, Syscall_Recvfrom, "recvfrom");

   function Syscall_Send
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int) return C.int;
   pragma Import (C, Syscall_Send, "send");

   function Syscall_Sendto
     (S     : C.int;
      Msg   : System.Address;
      Len   : C.int;
      Flags : C.int;
      To    : Sockaddr_In_Access;
      Tolen : C.int) return C.int;
   pragma Import (C, Syscall_Sendto, "sendto");

   function Syscall_Socket
     (Domain   : C.int;
      Typ      : C.int;
      Protocol : C.int) return C.int;
   pragma Import (C, Syscall_Socket, "socket");

   function  Non_Blocking_Socket (S : C.int) return Boolean;
   procedure Set_Non_Blocking_Socket (S : C.int; V : Boolean);

   --------------
   -- C_Accept --
   --------------

   function C_Accept
     (S       : C.int;
      Addr    : System.Address;
      Addrlen : access C.int) return C.int
   is
      R   : C.int;
      Val : aliased C.int := 1;

      Res : C.int;
      pragma Unreferenced (Res);

   begin
      loop
         R := Syscall_Accept (S, Addr, Addrlen);
         exit when Thread_Blocking_IO
           or else R /= Failure
           or else Non_Blocking_Socket (S)
           or else Errno /= Constants.EWOULDBLOCK;
         delay Quantum;
      end loop;

      if not Thread_Blocking_IO
        and then R /= Failure
      then
         --  A socket inherits the properties ot its server especially
         --  the FIONBIO flag. Do not use C_Ioctl as this subprogram
         --  tracks sockets set in non-blocking mode by user.

         Set_Non_Blocking_Socket (R, Non_Blocking_Socket (S));
         Res := Syscall_Ioctl (R, Constants.FIONBIO, Val'Unchecked_Access);
         --  Is it OK to ignore result ???
      end if;

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

      if Thread_Blocking_IO
        or else Res /= Failure
        or else Non_Blocking_Socket (S)
        or else Errno /= Constants.EINPROGRESS
      then
         return Res;
      end if;

      declare
         WSet : Fd_Set_Access;
         Now  : aliased Timeval;

      begin
         WSet := New_Socket_Set (No_Socket_Set);

         loop
            Insert_Socket_In_Set (WSet, S);
            Now := Immediat;
            Res := C_Select
              (S + 1,
               No_Fd_Set,
               WSet,
               No_Fd_Set,
               Now'Unchecked_Access);

            exit when Res > 0;

            if Res = Failure then
               Free_Socket_Set (WSet);
               return Res;
            end if;

            delay Quantum;
         end loop;

         Free_Socket_Set (WSet);
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

   ---------------------
   -- C_Gethostbyaddr --
   ---------------------

   function C_Gethostbyaddr
     (Addr : System.Address;
      Len  : C.int;
      Typ  : C.int) return Hostent_Access
   is
      pragma Warnings (Off, Len);
      pragma Warnings (Off, Typ);

      type int_Access is access int;
      function To_Pointer is
        new Unchecked_Conversion (System.Address, int_Access);

      procedure VxWorks_Gethostbyaddr
        (Addr : C.int; Buf : out C.char_array);
      pragma Import (C, VxWorks_Gethostbyaddr, "hostGetByAddr");

      Host_Name : C.char_array (1 .. Max_Name_Length);

   begin
      VxWorks_Gethostbyaddr (To_Pointer (Addr).all, Host_Name);

      In_Addr_Access_Ptr.all.all := To_In_Addr (To_Pointer (Addr).all);
      Local_Hostent.all.H_Name := C.Strings.New_Char_Array (Host_Name);

      return Local_Hostent;
   end C_Gethostbyaddr;

   ---------------------
   -- C_Gethostbyname --
   ---------------------

   function C_Gethostbyname
     (Name : C.char_array) return Hostent_Access
   is
      function VxWorks_Gethostbyname
        (Name : C.char_array) return C.int;
      pragma Import (C, VxWorks_Gethostbyname, "hostGetByName");

      Addr : C.int;

   begin
      Addr := VxWorks_Gethostbyname (Name);

      In_Addr_Access_Ptr.all.all := To_In_Addr (Addr);
      Local_Hostent.all.H_Name := C.Strings.New_Char_Array (To_C (Host_Name));

      return Local_Hostent;
   end C_Gethostbyname;

   ---------------------
   -- C_Getservbyname --
   ---------------------

   function C_Getservbyname
     (Name  : C.char_array;
      Proto : C.char_array) return Servent_Access
   is
      pragma Warnings (Off, Name);
      pragma Warnings (Off, Proto);

   begin
      return null;
   end C_Getservbyname;

   ---------------------
   -- C_Getservbyport --
   ---------------------

   function C_Getservbyport
     (Port  : C.int;
      Proto : C.char_array) return Servent_Access
   is
      pragma Warnings (Off, Port);
      pragma Warnings (Off, Proto);

   begin
      return null;
   end C_Getservbyport;

   -------------
   -- C_Ioctl --
   -------------

   function C_Ioctl
     (S    : C.int;
      Req  : C.int;
      Arg  : Int_Access) return C.int
   is
   begin
      if not Thread_Blocking_IO
        and then Req = Constants.FIONBIO
      then
         if Arg.all /= 0 then
            Set_Non_Blocking_Socket (S, True);
         end if;
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
      Flags : C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Recv (S, Msg, Len, Flags);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
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
      Fromlen : access C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Recvfrom (S, Msg, Len, Flags, From, Fromlen);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
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
      Flags : C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Send (S, Msg, Len, Flags);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
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
      Tolen : C.int) return C.int
   is
      Res : C.int;

   begin
      loop
         Res := Syscall_Sendto (S, Msg, Len, Flags, To, Tolen);
         exit when Thread_Blocking_IO
           or else Res /= Failure
           or else Non_Blocking_Socket (S)
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
      Protocol : C.int) return C.int
   is
      R   : C.int;
      Val : aliased C.int := 1;

      Res : C.int;
      pragma Unreferenced (Res);

   begin
      R := Syscall_Socket (Domain, Typ, Protocol);

      if not Thread_Blocking_IO
        and then R /= Failure
      then
         --  Do not use C_Ioctl as this subprogram tracks sockets set
         --  in non-blocking mode by user.

         Res := Syscall_Ioctl (R, Constants.FIONBIO, Val'Unchecked_Access);
         --  Is it OK to ignore result ???
         Set_Non_Blocking_Socket (R, False);
      end if;

      return R;
   end C_Socket;

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

   -------------------------
   -- Non_Blocking_Socket --
   -------------------------

   function Non_Blocking_Socket (S : C.int) return Boolean is
      R : Boolean;

   begin
      Task_Lock.Lock;
      R := (Is_Socket_In_Set (Non_Blocking_Sockets, S) /= 0);
      Task_Lock.Unlock;
      return R;
   end Non_Blocking_Socket;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Sin     : Sockaddr_In_Access;
      Address : In_Addr)
   is
   begin
      Sin.Sin_Addr   := Address;
   end Set_Address;

   ----------------
   -- Set_Family --
   ----------------

   procedure Set_Family
     (Sin    : Sockaddr_In_Access;
      Family : C.int)
   is
   begin
      Sin.Sin_Family := C.unsigned_char (Family);
   end Set_Family;

   ----------------
   -- Set_Length --
   ----------------

   procedure Set_Length
     (Sin : Sockaddr_In_Access;
      Len : C.int)
   is
   begin
      Sin.Sin_Length := C.unsigned_char (Len);
   end Set_Length;

   -----------------------------
   -- Set_Non_Blocking_Socket --
   -----------------------------

   procedure Set_Non_Blocking_Socket (S : C.int; V : Boolean) is
   begin
      Task_Lock.Lock;
      if V then
         Insert_Socket_In_Set (Non_Blocking_Sockets, S);
      else
         Remove_Socket_From_Set (Non_Blocking_Sockets, S);
      end if;

      Task_Lock.Unlock;
   end Set_Non_Blocking_Socket;

   --------------
   -- Set_Port --
   --------------

   procedure Set_Port
     (Sin  : Sockaddr_In_Access;
      Port : C.unsigned_short)
   is
   begin
      Sin.Sin_Port   := Port;
   end Set_Port;

   --------------------------
   -- Socket_Error_Message --
   --------------------------

   function Socket_Error_Message
     (Errno : Integer) return C.Strings.chars_ptr
   is
      use type Interfaces.C.Strings.chars_ptr;

      C_Msg : C.Strings.chars_ptr;

   begin
      C_Msg := C_Strerror (C.int (Errno));

      if C_Msg = C.Strings.Null_Ptr then
         return Unknown_System_Error;

      else
         return C_Msg;
      end if;
   end Socket_Error_Message;

--  Package elaboration

begin
   Local_Hostent.all.H_Aliases   := Alias_Access;

   --  VxWorks currently only supports AF_INET

   Local_Hostent.all.H_Addrtype  := Constants.AF_INET;

   Local_Hostent.all.H_Length    := 1;
   Local_Hostent.all.H_Addr_List := In_Addr_Access_Ptr;

end GNAT.Sockets.Thin;
