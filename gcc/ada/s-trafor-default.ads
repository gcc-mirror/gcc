------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                  S Y S T E M . T R A C E S . F O R M A T                 --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software;  you can redistribute it  and/or modify it under --
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

--  This package implements functions to format run-time traces

package System.Traces.Format is
   pragma Preelaborate;

   Max_Size : constant Integer := 128;
   --  Maximum size if event messages

   subtype String_Trace is String (1 .. Max_Size);
   --  Specific type in which trace information is stored. An ASCII.NUL
   --  character ends the string so that it is compatible with C strings
   --  which is useful on some targets (e.g. VxWorks)

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
