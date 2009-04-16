------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2008, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
