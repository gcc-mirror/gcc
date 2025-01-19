------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . S O C K E T S . P O L L . W A I T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2020-2025, AdaCore                   --
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

--  Wait implementation on top of posix select call

with GNAT.Sockets.Poll.G_Wait;

separate (GNAT.Sockets.Poll)

procedure Wait
  (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer)
is
   use Interfaces;

   function Get_Max_FD return FD_Type;
   --  Check is Max_FD is actual and correct it if necessary

   type FD_Set_Type is array (0 .. Get_Max_FD / C.long'Size) of C.long
     with Convention => C;

   procedure Reset_Socket_Set (Set : in out FD_Set_Type);
   --  Use own FD_ZERO routine because FD_Set_Type size depend on Fds.Max_FD

   procedure Insert_Socket_In_Set (Set : in out FD_Set_Type; FD : FD_Type)
     with Import, Convention => C,
          External_Name => "__gnat_insert_socket_in_set";

   function Is_Socket_In_Set (Set : FD_Set_Type; FD : FD_Type) return C.int
     with Import, Convention => C,
          External_Name => "__gnat_is_socket_in_set";

   procedure Reset_Socket_Set (Set : in out FD_Set_Type) is
   begin
      Set := (others => 0);
   end Reset_Socket_Set;

   procedure Poll is new G_Wait
     (FD_Set_Type, Reset_Socket_Set, Insert_Socket_In_Set, Is_Socket_In_Set);

   ----------------
   -- Get_Max_FD --
   ----------------

   function Get_Max_FD return FD_Type is
   begin
      if not Fds.Max_OK then
         Fds.Max_FD := Fds.Fds (Fds.Fds'First).Socket;

         for J in Fds.Fds'First + 1 .. Fds.Length loop
            if Fds.Max_FD < Fds.Fds (J).Socket then
               Fds.Max_FD := Fds.Fds (J).Socket;
            end if;
         end loop;

         Fds.Max_OK := True;
      end if;

      return Fds.Max_FD;
   end Get_Max_FD;

begin
   Poll (Fds, Timeout, Result);
end Wait;
