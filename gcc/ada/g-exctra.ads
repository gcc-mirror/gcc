------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  G N A T . E X C E P T I O N _ T R A C E S               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2000 Ada Core Technologies, Inc.              --
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

--  This package provides an interface allowing to control *automatic* output
--  to standard error upon exception occurrences (as opposed to explicit
--  generation of traceback information using GNAT.Traceback).
--
--  This output includes the basic information associated with the exception
--  (name, message) as well as a backtrace of the call chain at the point
--  where the exception occurred. This backtrace is only output if the call
--  chain information is available, depending if the binder switch dedicated
--  to that purpose has been used or not.
--
--  The default backtrace is in the form of absolute code locations which may
--  be converted to corresponding source locations using the addr2line utility
--  or from within GDB. Please refer to GNAT.Traceback for information about
--  what is necessary to be able to exploit thisg possibility.
--
--  The backtrace output can also be customized by way of a "decorator" which
--  may return any string output in association with a provided call chain.

with GNAT.Traceback; use GNAT.Traceback;

package GNAT.Exception_Traces is

   --  The following defines the exact situations in which raises will
   --  cause automatic output of trace information.

   type Trace_Kind is
     (Every_Raise,
      --  Denotes the initial raise event for any exception occurrence, either
      --  explicit or due to a specific language rule, within the context of a
      --  task or not.

      Unhandled_Raise
      --  Denotes the raise events corresponding to exceptions for which there
      --  is no user defined handler, in particular, when a task dies due to an
      --  unhandled exception.
     );

   --  The following procedures can be used to activate and deactivate
   --  traces identified by the above trace kind values.

   procedure Trace_On (Kind : in Trace_Kind);
   --  Activate the traces denoted by Kind.

   procedure Trace_Off;
   --  Stop the tracing requested by the last call to Trace_On.
   --  Has no effect if no such call has ever occurred.

   --  The following provide the backtrace decorating facilities

   type Traceback_Decorator is access
     function (Traceback : Tracebacks_Array) return String;
   --  A backtrace decorator is a function which returns the string to be
   --  output for a call chain provided by way of a tracebacks array.

   procedure Set_Trace_Decorator (Decorator : Traceback_Decorator);
   --  Set the decorator to be used for future automatic outputs. Restore
   --  the default behavior (output of raw addresses) if the provided
   --  access value is null.

end GNAT.Exception_Traces;
