------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         G N A T . S I G N A L S                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2003-2017, Free Software Foundation, Inc.         --
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

--  This package provides operations for querying and setting the blocked
--  status of signals.

--  This package is supported only on targets where Ada.Interrupts.Interrupt_ID
--  corresponds to software signals on the target, and where System.Interrupts
--  provides the ability to block and unblock signals.

with Ada.Interrupts;

package GNAT.Signals is

   procedure Block_Signal (Signal : Ada.Interrupts.Interrupt_ID);
   --  Block "Signal" at the process level

   procedure Unblock_Signal (Signal : Ada.Interrupts.Interrupt_ID);
   --  Unblock "Signal" at the process level

   function Is_Blocked (Signal : Ada.Interrupts.Interrupt_ID) return Boolean;
   --  "Signal" blocked at the process level?

end GNAT.Signals;
