------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                  S Y S T E M . T R A C E S . F O R M A T                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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

--  This package implements functions to format run-time traces

package System.Traces.Format is

   Max_Size : constant Integer := 128;
   --  Event messages' maximum size.

   subtype String_Trace is String (1 .. Max_Size);
   --  Specific type in which trace information is stored. An ASCII.NUL
   --  character ends the string so that it is compatible with C strings
   --  which is useful on some targets (eg. VxWorks)

   --  These private functions handles String_Trace formatting

   function Format_Trace (Source : String) return String_Trace;
   --  Put a String in a String_Trace, truncates the string if necessary.
   --  Similar to Head( .. ) found in Ada.Strings.Bounded

   function Append
     (Source : String_Trace;
      Annex  : String)
      return   String_Trace;
   pragma Inline (Append);
   --  Concatenates two string, similar to & operator from Ada.String.Unbounded

   procedure Send_Trace (Id : Trace_T; Info : String);
   --  This function (which is a subunit) send messages to external programs

end System.Traces.Format;
