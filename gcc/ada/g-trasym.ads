------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2012, AdaCore                     --
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

--  Run-time symbolic traceback support

--  The full capability is currently supported on the following targets:

--     HP-UX ia64
--     GNU/Linux x86, x86_64, ia64
--     FreeBSD x86, x86_64
--     Solaris sparc and x86
--     OpenVMS Alpha and ia64
--     Windows

--  Note: on targets other than those listed above, a dummy implementation of
--  the body returns a series of LF separated strings of the form "0x..."
--  corresponding to the addresses.

--  The routines provided in this package assume that your application has
--  been compiled with debugging information turned on, since this information
--  is used to build a symbolic traceback.

--  If you want to retrieve tracebacks from exception occurrences, it is also
--  necessary to invoke the binder with -E switch. Please refer to the gnatbind
--  documentation for more information.

--  Note that it is also possible (and often recommended) to compute symbolic
--  traceback outside the program execution, which in addition allows you
--  to distribute the executable with no debug info:
--
--  - build your executable with debug info
--  - archive this executable
--  - strip a copy of the executable and distribute/deploy this version
--  - at run time, compute absolute traceback (-bargs -E) from your
--    executable and log it using Ada.Exceptions.Exception_Information
--  - off line, compute the symbolic traceback using the executable archived
--    with debug info and addr2line or gdb (using info line *<addr>) on the
--    absolute addresses logged by your application.

--  In order to retrieve symbolic information, functions in this package will
--  read on disk all the debug information of the executable file (found via
--  Argument (0), and looked in the PATH if needed) or shared libraries using
--  OS facilities, and load them in memory, causing a significant cpu and
--  memory overhead.

--  Symbolic traceback from shared libraries is only supported for VMS, Windows
--  and GNU/Linux. On other targets symbolic tracebacks are only supported for
--  the main executable. You should consider using gdb to obtain symbolic
--  traceback in such cases.

--  On VMS, there is no restriction on using this facility with shared
--  libraries. However, the OS should be at least v7.3-1 and OS patch
--  VMS731_TRACE-V0100 must be applied in order to use this package.

--  On platforms where the full capability is not supported, function
--  Symbolic_Traceback return a list of addresses expressed as "0x..."
--  separated by line feed.

with Ada.Exceptions; use Ada.Exceptions;

package GNAT.Traceback.Symbolic is
   pragma Elaborate_Body;

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String;
   --  Build a string containing a symbolic traceback of the given call chain.
   --  Note: This procedure may be installed by Set_Trace_Decorator, to get a
   --  symbolic traceback on all exceptions raised (see GNAT.Exception_Traces).

   function Symbolic_Traceback (E : Exception_Occurrence) return String;
   --  Build string containing symbolic traceback of given exception occurrence

end GNAT.Traceback.Symbolic;
