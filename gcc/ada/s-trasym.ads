------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . T R A C E B A C K . S Y M B O L I C              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 1999-2017, AdaCore                     --
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

--  The routines provided in this package assume that your application has been
--  compiled with debugging information turned on, since this information is
--  used to build a symbolic traceback.

--  If you want to retrieve tracebacks from exception occurrences, it is also
--  necessary to invoke the binder with -E switch. Please refer to the gnatbind
--  documentation for more information.

--  Note that it is also possible (and often recommended) to compute symbolic
--  traceback outside the program execution, which in addition allows you to
--  distribute the executable with no debug info:
--
--     - build your executable with debug info
--     - archive this executable
--     - strip a copy of the executable and distribute/deploy this version
--     - at run time, compute absolute traceback (-bargs -E) from your
--       executable and log it using Ada.Exceptions.Exception_Information
--     - off line, compute the symbolic traceback using the executable archived
--       with debug info and addr2line or gdb (using info line *<addr>) on the
--       absolute addresses logged by your application.

--  In order to retrieve symbolic information, functions in this package will
--  read on disk all the debug information of the executable file (found via
--  Argument (0), and looked in the PATH if needed) or shared libraries using
--  OS facilities, and load them in memory, causing a significant cpu and
--  memory overhead.

pragma Polling (Off);
--  We must turn polling off for this unit, because otherwise we can get
--  elaboration circularities when polling is turned on.

with Ada.Exceptions;

package System.Traceback.Symbolic is
   pragma Elaborate_Body;

   function Symbolic_Traceback
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String;
   function Symbolic_Traceback_No_Hex
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String;
   --  Build a string containing a symbolic traceback of the given call
   --  chain. Note: These procedures may be installed by Set_Trace_Decorator,
   --  to get a symbolic traceback on all exceptions raised (see
   --  System.Exception_Traces).

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String;
   function Symbolic_Traceback_No_Hex
     (E : Ada.Exceptions.Exception_Occurrence) return String;
   --  Build string containing symbolic traceback of given exception occurrence

   --  In the above, _No_Hex means do not print any hexadecimal addresses, even
   --  if the symbol is not available. This is useful for getting deterministic
   --  output from tests.

   procedure Enable_Cache (Include_Modules : Boolean := False);
   --  Read symbolic information from binary files and cache them in memory.
   --  This will speed up the above functions but will require more memory. If
   --  Include_Modules is true, shared modules (or DLL) will also be cached.
   --  This procedure may do nothing if not supported. The profile of this
   --  subprogram may change in the future (new parameters can be added
   --  with default value), but backward compatibility for direct calls
   --  is supported.

end System.Traceback.Symbolic;
