------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2010, AdaCore                     --
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

--  Version used on unimplemented targets

--  Run-time symbolic traceback is currently supported on the following
--  targets:

--     HP-UX hppa and ia64
--     IRIX
--     GNU/Linux x86, x86_64, ia64
--     AIX
--     Solaris sparc and x86
--     Tru64
--     OpenVMS/Alpha
--     Windows NT/XP/Vista

--  This version is used on all other targets, it generates a warning at
--  compile time if it is with'ed, and the bodies generate messages saying
--  that the functions are not implemented.

with Ada.Exceptions; use Ada.Exceptions;

package GNAT.Traceback.Symbolic is
   pragma Elaborate_Body;

--     pragma Compile_Time_Warning
--       (True, "symbolic traceback not implemented on this target");

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String;
   --  Build a string containing a symbolic traceback of the given call chain

   function Symbolic_Traceback (E : Exception_Occurrence) return String;
   --  Build string containing symbolic traceback of given exception occurrence

end GNAT.Traceback.Symbolic;
