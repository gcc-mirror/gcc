------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       G N A T . T R A C E B A C K                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1999-2001 Ada Core Technologies, Inc.            --
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
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time non-symbolic traceback support

--  This package provides a method for generating a traceback of the
--  current execution location. The traceback shows the locations of
--  calls in the call chain, up to either the top or a designated
--  number of levels.

--  The traceback information is in the form of absolute code locations.
--  These code locations may be converted to corresponding source locations
--  using the external addr2line utility, or from within GDB.

--  To analyze the code locations later using addr2line or gdb, the necessary
--  units must be compiled with the debugging switch -g in the usual manner.
--  Note that it is not necessary to compile with -g to use Call_Chain. In
--  other words, the following sequence of steps can be used:

--     Compile without -g
--     Run the program, and call Call_Chain
--     Recompile with -g
--     Use addr2line to interpret the absolute call locations

--  This capability is currently supported on the following targets:

--     All x86 ports
--     AiX PowerPC
--     HP-UX
--     Irix
--     Solaris sparc
--     Tru64
--     VxWorks PowerPC
--     VxWorks Alpha

with System;

package GNAT.Traceback is
   pragma Elaborate_Body;

   subtype Code_Loc is System.Address;
   --  Code location used in building tracebacks

   type Tracebacks_Array is array (Positive range <>) of Code_Loc;
   --  Traceback array used to hold a generated traceback list.

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain (Traceback : out Tracebacks_Array; Len : out Natural);
   --  Store up to Traceback'Length tracebacks corresponding to the current
   --  call chain. The first entry stored corresponds to the deepest level
   --  of subprogram calls. Len shows the number of traceback entries stored.
   --  It will be equal to Traceback'Length unless the entire traceback is
   --  shorter, in which case positions in Traceback past the Len position
   --  are undefined on return.

end GNAT.Traceback;
