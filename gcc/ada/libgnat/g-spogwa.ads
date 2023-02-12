------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             G N A T . S O C K E T S . P O L L . G _ W A I T              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                       Copyright (C) 2020-2023, AdaCore                   --
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

with Interfaces.C;

private generic
   type FD_Set_Type is private;
   with procedure Reset_Socket_Set (Set : in out FD_Set_Type);
   with procedure Insert_Socket_In_Set
          (Set : in out FD_Set_Type; FD : FD_Type);
   with function Is_Socket_In_Set
          (Set : FD_Set_Type; FD : FD_Type) return Interfaces.C.int;
procedure GNAT.Sockets.Poll.G_Wait
  (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer);
--  Common code to implement GNAT.Sockets.Poll.Wait routine on top of posix or
--  win32 select API.
--  Posix and Win32 select has the same API but different socket set structure.
--  C API for select has socket set size defined at compilation stage. This Ada
--  implementation allow to define size of socket set at the execution time.
--  Unlike C select API we do not need allocate socket set for maximum number
--  of sockets when we need to check only few of them. And we are not limited
--  with FD_SETSIZE when we need more sockets to check.
