------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             G N A T . S O C K E T S . P O L L . G _ W A I T              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2020-2024, AdaCore                   --
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

with GNAT.Sockets.Thin_Common;

procedure GNAT.Sockets.Poll.G_Wait
  (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer)
is
   use Interfaces;

   function C_Select
     (Nfds      : C.int;
      readfds   : access FD_Set_Type;
      writefds  : access FD_Set_Type;
      exceptfds : access FD_Set_Type;
      timeout   : access Thin_Common.Timeval) return Integer
     with Import => True, Convention => Stdcall, External_Name => "select";

   Timeout_V : aliased Thin_Common.Timeval;
   Timeout_A : access Thin_Common.Timeval;

   Rfds      : aliased FD_Set_Type;
   Rcount    : Natural := 0;
   Wfds      : aliased FD_Set_Type;
   Wcount    : Natural := 0;
   Efds      : aliased FD_Set_Type;

   Rfdsa     : access FD_Set_Type;
   Wfdsa     : access FD_Set_Type;

   FD_Events : Events_Type;

begin
   --  Setup (convert data from poll to select layout)

   if Timeout >= 0 then
      Timeout_A := Timeout_V'Access;
      Timeout_V.Tv_Sec  := Thin_Common.time_t  (Timeout / 1000);
      Timeout_V.Tv_Usec := Thin_Common.suseconds_t (Timeout rem 1000 * 1000);
   end if;

   Reset_Socket_Set (Rfds);
   Reset_Socket_Set (Wfds);
   Reset_Socket_Set (Efds);

   for J in Fds.Fds'First .. Fds.Length loop
      Fds.Fds (J).REvents := 0;

      FD_Events := Fds.Fds (J).Events;

      if (FD_Events and (SOC.POLLIN or SOC.POLLPRI)) /= 0 then
         Insert_Socket_In_Set (Rfds, Fds.Fds (J).Socket);
         Rcount := Rcount + 1;
      end if;

      if (FD_Events and SOC.POLLOUT) /= 0 then
         Insert_Socket_In_Set (Wfds, Fds.Fds (J).Socket);
         Wcount := Wcount + 1;
      end if;

      Insert_Socket_In_Set (Efds, Fds.Fds (J).Socket);

      if Fds.Fds (J).Socket > Fds.Max_FD then
         raise Program_Error with "Wrong Max_FD";
      end if;
   end loop;

   --  Any non-null descriptor set must contain at least one handle
   --  to a socket on Windows (MSDN).

   if Rcount /= 0 then
      Rfdsa := Rfds'Access;
   end if;

   if Wcount /= 0 then
      Wfdsa := Wfds'Access;
   end if;

   --  Call OS select

   Result :=
     C_Select (C.int (Fds.Max_FD + 1), Rfdsa, Wfdsa, Efds'Access, Timeout_A);

   --  Build result (convert back from select to poll layout)

   if Result > 0 then
      Result := 0;

      for J in Fds.Fds'First .. Fds.Length loop
         if Is_Socket_In_Set (Rfds, Fds.Fds (J).Socket) /= 0 then
            --  Do not need "or" with Poll_Ptr (J).REvents because it's zero

            Fds.Fds (J).REvents := SOC.POLLIN;
         end if;

         if Is_Socket_In_Set (Wfds, Fds.Fds (J).Socket) /= 0 then
            Fds.Fds (J).REvents := Fds.Fds (J).REvents or SOC.POLLOUT;
         end if;

         if Is_Socket_In_Set (Efds, Fds.Fds (J).Socket) /= 0 then
            Fds.Fds (J).REvents := Fds.Fds (J).REvents or SOC.POLLERR;
         end if;

         if Fds.Fds (J).REvents /= 0 then
            Result := Result + 1;
         end if;
      end loop;
   end if;
end GNAT.Sockets.Poll.G_Wait;
