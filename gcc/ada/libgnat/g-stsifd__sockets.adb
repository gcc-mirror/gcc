------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--     G N A T . S O C K E T S . T H I N . S I G N A L L I N G _ F D S      --
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
      Res : constant C.int :=
              C_Socketpair (SOSC.AF_INET, SOSC.SOCK_STREAM, 0, Fds);
   begin
      if Res /= Failure then
         --  Set TCP_NODELAY on Fds (Write_End), since we always want to send
         --  the data out immediately.

         Set_Socket_Option
           (Socket => Socket_Type (Fds (Write_End)),
            Level  => IP_Protocol_For_TCP_Level,
            Option => (Name => No_Delay, Enabled => True));
      end if;

      return Res;
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
