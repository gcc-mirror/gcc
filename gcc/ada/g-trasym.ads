------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--             G N A T . T R A C E B A C K . S Y M B O L I C                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 1999-2004 Ada Core Technologies, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Run-time symbolic traceback support

--  Note: this is only available on selected targets. Currently it is
--  supported on Sparc/Solaris, GNU/Linux, Windows NT, HP-UX, VMS and Tru64.

--  The routines provided in this package assume that your application has
--  been compiled with debugging information turned on, since this information
--  is used to build a symbolic traceback.

--  It is also in some cases necessary to invoke the binder
--  with -E switch (store the backtrace with exception occurence). Please
--  refer to gnatbind documentation for more information.

--  In order to retrieve symbolic information, functions in this package will
--  read on disk all the debug information of the executable file (found via
--  Argument (0), so any path information needed to read the executable file
--  need to be provided when launching the executable), and load then in
--  memory, causing a significant cpu and memory overhead.

--  On all platforms except VMS, this package is not intended to be used
--  within a shared library, symbolic tracebacks are only supported for the
--  main executable and not for shared libraries. You should consider using
--  gdb to obtain symbolic traceback in such cases.

--  On VMS, there is no restriction on using this facility with shared
--  libraries. However, the OS should be at least v7.3-1 and OS patch
--  VMS731_TRACE-V0100 must be applied in order to use this package.

with Ada.Exceptions; use Ada.Exceptions;

package GNAT.Traceback.Symbolic is
pragma Elaborate_Body (Traceback.Symbolic);

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback (Traceback : Tracebacks_Array) return String;
   --  Build a string containing a symbolic traceback of the given call chain

   function Symbolic_Traceback (E : Exception_Occurrence) return String;
   --  Build string containing symbolic traceback of given exception occurrence

end GNAT.Traceback.Symbolic;
