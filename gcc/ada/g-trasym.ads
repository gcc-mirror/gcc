------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1999-2002 Ada Core Technologies, Inc.            --
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

--  Run-time symbolic traceback support

--  Note: this is only available on selected targets. Currently it is
--  supported on Sparc/Solaris, GNU/Linux, Windows NT, HP-UX and Tru64.

--  The routines provided in this package assume that your application has
--  been compiled with debugging information turned on, since this information
--  is used to build a symbolic traceback.
--
--  In order to retrieve symbolic information, functions in this package will
--  read on disk all the debug information of the current executable and load
--  them in memory, causing a significant cpu and memory overhead.
--
--  This package is not intended to be used within a shared library,
--  symbolic tracebacks are only supported for the main executable
--  and not for shared libraries.
--
--  You should consider using off-line symbolic traceback instead, using
--  addr2line or gdb.

with Ada.Exceptions; use Ada.Exceptions;

package GNAT.Traceback.Symbolic is
pragma Elaborate_Body (Traceback.Symbolic);

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String;
   --  Build a string containing a symbolic traceback of the given call chain.

   function Symbolic_Traceback (E : Exception_Occurrence) return String;
   --  Build a string containing a symbolic traceback of the given exception
   --  occurrence.

end GNAT.Traceback.Symbolic;
