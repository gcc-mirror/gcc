------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . S O C K E T S . P O L L . W A I T               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                       Copyright (C) 2020-2021, AdaCore                   --
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

--  Wait implementation on top of Windows select call
--
--  Microsoft Windows from Vista version has WSAPoll function in API which is
--  similar to POSIX poll call, but experiments show that the WSAPoll is much
--  slower than select at least in Windows version 10.0.18363.1016.

with GNAT.Sockets.Poll.G_Wait;

separate (GNAT.Sockets.Poll)

procedure Wait
  (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer)
is
   use Interfaces;

   type FD_Array is array (1 .. Fds.Length) of FD_Type
     with Convention => C;

   type FD_Set_Type is record
      Count : C.int;
      Set   : FD_Array;
   end record with Convention => C;

   procedure Reset_Socket_Set (Set : in out FD_Set_Type) with Inline;

   procedure Insert_Socket_In_Set (Set : in out FD_Set_Type; FD : FD_Type)
     with Inline;

   function Is_Socket_In_Set (Set : FD_Set_Type; FD : FD_Type) return C.int
     with Import, Convention => C,
          External_Name => "__gnat_is_socket_in_set";

   --------------------------
   -- Insert_Socket_In_Set --
   --------------------------

   procedure Insert_Socket_In_Set (Set : in out FD_Set_Type; FD : FD_Type) is
   begin
      Set.Count := Set.Count + 1;
      Set.Set (Integer (Set.Count)) := FD;
   end Insert_Socket_In_Set;

   ----------------------
   -- Reset_Socket_Set --
   ----------------------

   procedure Reset_Socket_Set (Set : in out FD_Set_Type) is
   begin
      Set.Count := 0;
   end Reset_Socket_Set;

   ----------
   -- Poll --
   ----------

   procedure Poll is new G_Wait
     (FD_Set_Type, Reset_Socket_Set, Insert_Socket_In_Set, Is_Socket_In_Set);

begin
   Poll (Fds, Timeout, Result);
end Wait;
