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

--  Wait implementation on top of native poll call
--
--  This submodule can be used on systems where poll system call is natively
--  supported. Microsoft Windows supports WSAPoll system call from Vista
--  version and this submodule can be used on such Windows versions too, the
--  System.OS_Constants.Poll_Linkname constant defines appropriate link name
--  for Windows. But we do not use WSAPoll in GNAT.Sockets.Poll implementation
--  for now because it is much slower than select system call, at least in
--  Windows version 10.0.18363.1016.

separate (GNAT.Sockets.Poll)

procedure Wait
  (Fds : in out Set; Timeout : Interfaces.C.int; Result : out Integer)
is

   function Poll
     (Fds     : Poll_Set;
      Nfds    : nfds_t;
      Timeout : Interfaces.C.int) return Integer
     with Import, Convention => Stdcall, External_Name => SOC.Poll_Linkname;

begin
   Result := Poll (Fds.Fds, nfds_t (Fds.Length), Timeout);
end Wait;
